use std::sync::OnceLock;

use pest::pratt_parser::PrattParser;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LValue {
	Variable {
		loc: Location,
		name: Path,
	},
	Accessor {
		loc: Location,
		lhs: Box<LValue>,
		rhs: Identifier,
	},
	Indexer {
		loc: Location,
		lhs: Box<LValue>,
		index: Expression,
	},
	Deref {
		loc: Location,
		rhs: Box<LValue>,
	},
}

impl AstNode for LValue {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		static PRATT: OnceLock<PrattParser<Rule>> = OnceLock::new();
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::lvalue);
		let lvalue = PRATT
			.get_or_init(|| {
				use pest::pratt_parser::Op;
				PrattParser::new()
					.op(Op::prefix(Rule::deref))
					.op(Op::postfix(Rule::accessor) | Op::postfix(Rule::indexer))
			})
			.map_primary(|prim| match prim.as_rule() {
				Rule::path => LValue::Variable {
					loc: loc.clone(),
					name: Path::build(ctx.clone(), prim),
				},
				Rule::lvalue_parent => {
					let mut children = prim.into_inner();
					children.expect_next_rule(Rule::lparent);
					LValue::build(ctx.clone(), children.expect_next())
				}
				_ => unreachable!(),
			})
			.map_postfix(|lhs, op| match op.as_rule() {
				Rule::accessor => {
					let mut children = op.into_inner();
					children.expect_next_rule(Rule::dot);
					LValue::Accessor {
						loc: loc.clone(),
						lhs: Box::new(lhs),
						rhs: Identifier::build(ctx.clone(), children.expect_next()),
					}
				}
				Rule::indexer => {
					let mut children = op.into_inner();
					children.expect_next_rule(Rule::lsquare);
					let index = Expression::build(ctx.clone(), children.expect_next());
					children.expect_next_rule(Rule::rsquare);
					LValue::Indexer {
						loc: loc.clone(),
						lhs: Box::new(lhs),
						index,
					}
				}
				_ => unreachable!(),
			})
			.map_prefix(|op, rhs| match op.as_rule() {
				Rule::deref => LValue::Deref {
					loc: loc.clone(),
					rhs: Box::new(rhs),
				},
				_ => unreachable!(),
			})
			.parse(parent.into_inner());
		lvalue
	}
	fn loc(&self) -> &Location {
		match self {
			LValue::Variable { loc, .. }
			| LValue::Accessor { loc, .. }
			| LValue::Indexer { loc, .. }
			| LValue::Deref { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			LValue::Variable { loc, .. }
			| LValue::Accessor { loc, .. }
			| LValue::Indexer { loc, .. }
			| LValue::Deref { loc, .. } => loc,
		}
	}
}

impl Naive for LValue {
	type Output = NaiveLValue;
	fn naive(self) -> Self::Output {
		NaiveLValue::from(self)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_simple() {
		let (res, _): (LValue, _) = build!(r#"x"#, lvalue);
		assert_eq!(res.naive(), lvar(["x"]));
	}

	#[test]
	fn test_deref() {
		let (res, _): (LValue, _) = build!(r#"*x"#, lvalue);
		assert_eq!(res.naive(), lderef(lvar(["x"])));
	}

	#[test]
	fn test_index() {
		let (res, _): (LValue, _) = build!(r#"x[5]"#, lvalue);
		assert_eq!(res.naive(), lindexer(lvar(["x"]), integer(5)));
	}

	#[test]
	fn test_accessor() {
		let (res, _): (LValue, _) = build!(r#"x.y"#, lvalue);
		assert_eq!(res.naive(), laccessor(lvar(["x"]), "y"));
	}

	#[test]
	fn test_mixed() {
		let (res, _): (LValue, _) = build!(r#"*a.c"#, lvalue);
		assert_eq!(res.naive(), lderef(laccessor(lvar(["a"]), "c")));
	}

	#[test]
	fn test_complex() {
		let (res, _): (LValue, _) = build!(r#"*(**a::b.c).d.e"#, lvalue);
		assert_eq!(
			res.naive(),
			lderef(laccessor(
				laccessor(lderef(lderef(laccessor(lvar(["a", "b"]), "c"))), "d"),
				"e"
			))
		);
	}
}
