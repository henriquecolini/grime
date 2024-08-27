use std::sync::OnceLock;

use pest::pratt_parser::PrattParser;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
	Integer {
		loc: Location,
		value: i64,
	},
	String {
		loc: Location,
		value: Vec<u8>,
	},
	Boolean {
		loc: Location,
		value: bool,
	},
	Array {
		loc: Location,
		value: Vec<Expression>,
	},
	FunctionCall {
		loc: Location,
		value: FunctionCall,
	},
	Variable {
		loc: Location,
		value: Path,
	},
	BinaryOp {
		loc: Location,
		op: BinaryOperator,
		left: Box<Expression>,
		right: Box<Expression>,
	},
	UnaryOp {
		loc: Location,
		op: UnaryOperator,
		expr: Box<Expression>,
	},
	Accessor {
		loc: Location,
		base: Box<Expression>,
		field: Identifier,
	},
	Slicer {
		loc: Location,
		base: Box<Expression>,
		from: Option<Box<Expression>>,
		until: Option<Box<Expression>>,
	},
	Indexer {
		loc: Location,
		base: Box<Expression>,
		index: Box<Expression>,
	},
}

impl AstNode for Expression {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		static PRATT: OnceLock<PrattParser<Rule>> = OnceLock::new();
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::expression);
		let expr = PRATT
			.get_or_init(|| {
				use pest::pratt_parser::{Assoc::*, Op};
				PrattParser::new()
					.op(Op::infix(Rule::or, Left))
					.op(Op::infix(Rule::and, Left))
					.op(Op::infix(Rule::lt, Left)
						| Op::infix(Rule::gt, Left)
						| Op::infix(Rule::leq, Left)
						| Op::infix(Rule::geq, Left))
					.op(Op::infix(Rule::eq, Left) | Op::infix(Rule::neq, Left))
					.op(Op::infix(Rule::bit_or, Left))
					.op(Op::infix(Rule::bit_xor, Left))
					.op(Op::infix(Rule::bit_and, Left))
					.op(Op::infix(Rule::lshift, Left) | Op::infix(Rule::rshift, Left))
					.op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
					.op(Op::infix(Rule::multiply, Left)
						| Op::infix(Rule::divide, Left)
						| Op::infix(Rule::modulo, Left))
					.op(Op::prefix(Rule::neg)
						| Op::prefix(Rule::not) | Op::prefix(Rule::deref)
						| Op::prefix(Rule::r#ref)
						| Op::prefix(Rule::ref_const))
					.op(Op::postfix(Rule::indexer) | Op::postfix(Rule::slicer))
					.op(Op::postfix(Rule::accessor))
			})
			.map_primary(|prim| {
				let loc = loc.clone();
				match prim.as_rule() {
					Rule::integer => {
						let child = prim.into_inner().expect_single();
						match child.as_rule() {
							Rule::hex_integer => {
								let hex = child.as_str();
								let value = i64::from_str_radix(&hex[2..], 16).unwrap();
								Expression::Integer { loc, value }
							}
							Rule::bin_integer => {
								let bin = child.as_str();
								let value = i64::from_str_radix(&bin[2..], 2).unwrap();
								Expression::Integer { loc, value }
							}
							Rule::oct_integer => {
								let oct = child.as_str();
								let value = i64::from_str_radix(&oct[2..], 8).unwrap();
								Expression::Integer { loc, value }
							}
							Rule::dec_integer => {
								let dec = child.as_str();
								let value = dec.parse().unwrap();
								Expression::Integer { loc, value }
							}
							_ => unreachable!(),
						}
					}
					Rule::string => {
						let child = prim.into_inner().expect_single();
						child.expect_rule(Rule::inner);
						let mut value = vec![];
						for byte in child.into_inner() {
							match byte.as_str().as_bytes() {
								br"\0" => value.push(b'\0'),
								br"\t" => value.push(b'\t'),
								br"\n" => value.push(b'\n'),
								br"\r" => value.push(b'\r'),
								br"\" => value.push(b'\"'),
								br"\\" => value.push(b'\\'),
								[b'\\', b'x', byte @ ..] => {
									let hex = std::str::from_utf8(byte).unwrap();
									let num = u8::from_str_radix(hex, 16).unwrap();
									value.push(num);
								}
								_ => value.push(byte.as_str().as_bytes()[0]),
							}
						}
						Expression::String { loc, value }
					}
					Rule::boolean => {
						let child = prim.into_inner().expect_single();
						match child.as_str() {
							"true" => Expression::Boolean { loc, value: true },
							"false" => Expression::Boolean { loc, value: false },
							_ => unreachable!(),
						}
					}
					Rule::array => {
						let mut children = prim.into_inner();
						children.expect_next_rule(Rule::lsquare);
						let mut value = vec![];
						for child in children {
							if child.as_rule() == Rule::rsquare {
								break;
							}
							if child.as_rule() == Rule::comma {
								continue;
							}
							value.push(Expression::build(ctx.clone(), child));
						}
						Expression::Array { loc, value }
					}
					Rule::func_call => Expression::FunctionCall {
						loc,
						value: FunctionCall::build(ctx.clone(), prim),
					},
					Rule::path => Expression::Variable {
						loc,
						value: Path::build(ctx.clone(), prim),
					},
					Rule::exp_parent => {
						let mut children = prim.into_inner();
						children.expect_next_rule(Rule::lparent);
						Expression::build(ctx.clone(), children.expect_next())
					}
					_ => unreachable!(),
				}
			})
			.map_prefix(|op, lhs| {
				let loc = loc.clone();
				let op = op.as_rule();
				match op {
					Rule::neg => Expression::UnaryOp {
						loc,
						op: UnaryOperator::Negate,
						expr: Box::new(lhs),
					},
					Rule::not => Expression::UnaryOp {
						loc,
						op: UnaryOperator::Not,
						expr: Box::new(lhs),
					},
					Rule::deref => Expression::UnaryOp {
						loc,
						op: UnaryOperator::Deref,
						expr: Box::new(lhs),
					},
					Rule::r#ref => Expression::UnaryOp {
						loc,
						op: UnaryOperator::Ref,
						expr: Box::new(lhs),
					},
					Rule::ref_const => Expression::UnaryOp {
						loc,
						op: UnaryOperator::RefConst,
						expr: Box::new(lhs),
					},
					_ => unreachable!(),
				}
			})
			.map_infix(|lhs, op, rhs| {
				let loc = loc.clone();
				let op = op.as_rule();
				match op {
					Rule::or => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Or,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::and => Expression::BinaryOp {
						loc,
						op: BinaryOperator::And,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::lt => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Lt,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::gt => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Gt,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::leq => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Leq,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::geq => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Geq,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::eq => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Eq,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::neq => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Neq,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::bit_or => Expression::BinaryOp {
						loc,
						op: BinaryOperator::BitOr,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::bit_xor => Expression::BinaryOp {
						loc,
						op: BinaryOperator::BitXor,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::bit_and => Expression::BinaryOp {
						loc,
						op: BinaryOperator::BitAnd,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::lshift => Expression::BinaryOp {
						loc,
						op: BinaryOperator::LShift,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::rshift => Expression::BinaryOp {
						loc,
						op: BinaryOperator::RShift,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::add => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Add,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::subtract => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Subtract,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::multiply => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Multiply,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::divide => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Divide,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					Rule::modulo => Expression::BinaryOp {
						loc,
						op: BinaryOperator::Modulo,
						left: Box::new(lhs),
						right: Box::new(rhs),
					},
					_ => unreachable!(),
				}
			})
			.map_postfix(|lhs, op| {
				let loc = loc.clone();
				match op.as_rule() {
					Rule::accessor => {
						let mut children = op.into_inner();
						children.expect_next_rule(Rule::dot);
						Expression::Accessor {
							loc,
							base: Box::new(lhs),
							field: Identifier::build(ctx.clone(), children.expect_next()),
						}
					}
					Rule::indexer => {
						let mut children = op.into_inner();
						children.expect_next_rule(Rule::lsquare);
						let index = Expression::build(ctx.clone(), children.expect_next());
						children.expect_next_rule(Rule::rsquare);
						Expression::Indexer {
							loc,
							base: Box::new(lhs),
							index: Box::new(index),
						}
					}
					Rule::slicer => {
						let mut children = op.into_inner();
						children.expect_next_rule(Rule::lsquare);
						let from = if children.peek().map(|p| p.as_rule()) == Some(Rule::range) {
							children.next();
							None
						} else {
							let from = Expression::build(ctx.clone(), children.expect_next());
							children.expect_next_rule(Rule::range);
							Some(Box::new(from))
						};
						let until = if children.peek().map(|p| p.as_rule()) == Some(Rule::rsquare) {
							None
						} else {
							let until = Expression::build(ctx.clone(), children.expect_next());
							Some(Box::new(until))
						};
						children.expect_next_rule(Rule::rsquare);
						Expression::Slicer {
							loc,
							base: Box::new(lhs),
							from,
							until,
						}
					}
					_ => unreachable!(),
				}
			})
			.parse(parent.into_inner());
		expr
	}
	fn loc(&self) -> &Location {
		match self {
			Expression::Integer { loc, .. }
			| Expression::String { loc, .. }
			| Expression::Boolean { loc, .. }
			| Expression::Array { loc, .. }
			| Expression::FunctionCall { loc, .. }
			| Expression::Variable { loc, .. }
			| Expression::BinaryOp { loc, .. }
			| Expression::UnaryOp { loc, .. }
			| Expression::Accessor { loc, .. }
			| Expression::Slicer { loc, .. }
			| Expression::Indexer { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			Expression::Integer { loc, .. }
			| Expression::String { loc, .. }
			| Expression::Boolean { loc, .. }
			| Expression::Array { loc, .. }
			| Expression::FunctionCall { loc, .. }
			| Expression::Variable { loc, .. }
			| Expression::BinaryOp { loc, .. }
			| Expression::UnaryOp { loc, .. }
			| Expression::Accessor { loc, .. }
			| Expression::Slicer { loc, .. }
			| Expression::Indexer { loc, .. } => loc,
		}
	}
}

impl Naive for Expression {
	type Output = NaiveExpression;
	fn naive(self) -> NaiveExpression {
		self.into()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_dec_integer() {
		let (res, _): (Expression, _) = build!(r#"42"#, expression);
		assert_eq!(res.naive(), integer(42));
	}

	#[test]
	fn test_hex_integer() {
		let (res, _): (Expression, _) = build!(r#"0x2A"#, expression);
		assert_eq!(res.naive(), integer(0x2A));
	}

	#[test]
	fn test_bin_integer() {
		let (res, _): (Expression, _) = build!(r#"0b101010"#, expression);
		assert_eq!(res.naive(), integer(0b101010));
	}

	#[test]
	fn test_oct_integer() {
		let (res, _): (Expression, _) = build!(r#"0o52"#, expression);
		assert_eq!(res.naive(), integer(0o52));
	}

	#[test]
	fn test_string() {
		let (res, _): (Expression, _) =
			build!(r#""Hello, World!\n\t\r\0\\\xAA\xBB\xCC""#, expression);
		assert_eq!(res.naive(), string(b"Hello, World!\n\t\r\0\\\xAA\xBB\xCC"));
	}

	#[test]
	fn test_boolean() {
		let (res, _): (Expression, _) = build!(r#"true"#, expression);
		assert_eq!(res.naive(), boolean(true));

		let (res, _): (Expression, _) = build!(r#"false"#, expression);
		assert_eq!(res.naive(), boolean(false));
	}

	#[test]
	fn test_array() {
		let (res, _): (Expression, _) = build!(r#"[]"#, expression);
		assert_eq!(res.naive(), array([]));

		let (res, _): (Expression, _) = build!(r#"[1, 2, 3]"#, expression);
		assert_eq!(res.naive(), array([integer(1), integer(2), integer(3),]));
	}

	#[test]
	fn test_func_call() {
		let (res, _): (Expression, _) = build!(r#"foo()"#, expression);
		assert_eq!(res.naive(), call(["foo"], []));

		let (res, _): (Expression, _) = build!(r#"foo(bar, 42)"#, expression);
		assert_eq!(res.naive(), call(["foo"], [var(["bar"]), integer(42),]));

		let (res, _): (Expression, _) = build!(r#"foo::bar()"#, expression);
		assert_eq!(res.naive(), call(["foo", "bar"], []));
	}

	#[test]
	fn test_path() {
		let (res, _): (Expression, _) = build!(r#"foo::bar"#, expression);
		assert_eq!(res.naive(), var(["foo", "bar"]));
	}

	#[test]
	fn test_exp_parent() {
		let (res, _): (Expression, _) = build!(r#"(((42)))"#, expression);
		assert_eq!(res.naive(), integer(42));
	}

	macro_rules! test_unary_op {
		($name:ident, $input:expr, $op:expr) => {
			#[test]
			fn $name() {
				let (res, _): (Expression, _) = build!($input, expression);
				assert_eq!(res.naive(), unary($op, integer(42)));
			}
		};
	}

	test_unary_op!(test_negate, r#"-42"#, UnaryOperator::Negate);
	test_unary_op!(test_not, r#"!42"#, UnaryOperator::Not);
	test_unary_op!(test_deref, r#"*42"#, UnaryOperator::Deref);
	test_unary_op!(test_ref, r#"&42"#, UnaryOperator::Ref);
	test_unary_op!(test_ref_const, r#"&const 42"#, UnaryOperator::RefConst);

	macro_rules! test_binary_op {
		($name:ident, $input:expr, $op:expr) => {
			#[test]
			fn $name() {
				let (res, _): (Expression, _) = build!($input, expression);
				assert_eq!(res.naive(), binary($op, integer(42), integer(42)));
			}
		};
	}

	test_binary_op!(test_or, r#"42 || 42"#, BinaryOperator::Or);
	test_binary_op!(test_and, r#"42 && 42"#, BinaryOperator::And);
	test_binary_op!(test_lt, r#"42 < 42"#, BinaryOperator::Lt);
	test_binary_op!(test_gt, r#"42 > 42"#, BinaryOperator::Gt);
	test_binary_op!(test_leq, r#"42 <= 42"#, BinaryOperator::Leq);
	test_binary_op!(test_geq, r#"42 >= 42"#, BinaryOperator::Geq);
	test_binary_op!(test_eq, r#"42 == 42"#, BinaryOperator::Eq);
	test_binary_op!(test_neq, r#"42 != 42"#, BinaryOperator::Neq);
	test_binary_op!(test_bit_or, r#"42 | 42"#, BinaryOperator::BitOr);
	test_binary_op!(test_bit_xor, r#"42 ^ 42"#, BinaryOperator::BitXor);
	test_binary_op!(test_bit_and, r#"42 & 42"#, BinaryOperator::BitAnd);
	test_binary_op!(test_lshift, r#"42 << 42"#, BinaryOperator::LShift);
	test_binary_op!(test_rshift, r#"42 >> 42"#, BinaryOperator::RShift);
	test_binary_op!(test_add, r#"42 + 42"#, BinaryOperator::Add);
	test_binary_op!(test_subtract, r#"42 - 42"#, BinaryOperator::Subtract);
	test_binary_op!(test_multiply, r#"42 * 42"#, BinaryOperator::Multiply);
	test_binary_op!(test_divide, r#"42 / 42"#, BinaryOperator::Divide);
	test_binary_op!(test_modulo, r#"42 % 42"#, BinaryOperator::Modulo);

	#[test]
	fn test_precedence_mult() {
		let (res, _): (Expression, _) = build!(r#"42 + 42 * 42"#, expression);
		assert_eq!(
			res.naive(),
			binary(
				BinaryOperator::Add,
				integer(42),
				binary(BinaryOperator::Multiply, integer(42), integer(42))
			)
		);
	}

	#[test]
	fn test_precedence_sub() {
		let (res, _): (Expression, _) = build!(r#"42 - -42"#, expression);
		assert_eq!(
			res.naive(),
			binary(
				BinaryOperator::Subtract,
				integer(42),
				unary(UnaryOperator::Negate, integer(42))
			)
		);
	}

	macro_rules! test_postfix_op {
		($name:ident, $input:expr, $ex:expr) => {
			#[test]
			fn $name() {
				let (res, _): (Expression, _) = build!($input, expression);
				assert_eq!(res.naive(), $ex);
			}
		};
	}

	test_postfix_op!(test_accessor, r#"42.foo"#, accessor(integer(42), "foo"));

	test_postfix_op!(test_indexer, r#"42[42]"#, indexer(integer(42), integer(42)));

	test_postfix_op!(test_slicer, r#"42[..]"#, slicer(integer(42), None, None));

	test_postfix_op!(
		test_slicer_from,
		r#"42[1..]"#,
		slicer(integer(42), Some(integer(1)), None)
	);

	test_postfix_op!(
		test_slicer_until,
		r#"42[..1]"#,
		slicer(integer(42), None, Some(integer(1)))
	);

	test_postfix_op!(
		test_slicer_both,
		r#"42[1..1]"#,
		slicer(integer(42), Some(integer(1)), Some(integer(1)))
	);

	#[test]
	fn test_complex() {
		let (res, _): (Expression, _) =
			build!(r#"foo(&bar[42].baz, "abc") + qu[10] / 2"#, expression);
		assert_eq!(
			res.naive(),
			binary(
				BinaryOperator::Add,
				call(
					["foo"],
					[
						unary(
							UnaryOperator::Ref,
							accessor(indexer(var(["bar"]), integer(42)), "baz")
						),
						string(b"abc"),
					]
				),
				binary(
					BinaryOperator::Divide,
					indexer(var(["qu"]), integer(10)),
					integer(2)
				)
			)
		);
	}
}
