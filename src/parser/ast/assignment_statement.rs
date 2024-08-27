use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignStatement {
	pub loc: Location,
	pub lvalue: LValue,
	pub value: Expression,
}

impl AstNode for AssignStatement {
	fn build(ctx: Context, pair: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&pair);
		pair.expect_rule(Rule::assign_statement);
		let mut children = pair.into_inner();
		let lvalue = LValue::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::equals);
		let value = Expression::build(ctx, children.expect_next());
		AssignStatement { loc, lvalue, value }
	}
	fn loc(&self) -> &Location {
		&self.loc
	}
	fn loc_mut(&mut self) -> &mut Location {
		&mut self.loc
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_simple() {
		let (res, _): (AssignStatement, _) = build!(r#"x = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lvar(["x"]));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_path() {
		let (res, _): (AssignStatement, _) = build!(r#"a::b::c = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lvar(["a", "b", "c"]));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_index() {
		let (res, _): (AssignStatement, _) = build!(r#"a[0] = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lindexer(lvar(["a"]), integer(0)));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_ref() {
		let (res, _): (AssignStatement, _) = build!(r#"*a = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lderef(lvar(["a"])));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_ref_index() {
		let (res, _): (AssignStatement, _) = build!(r#"*a[0] = 5;"#, assign_statement);
		assert_eq!(
			res.lvalue.naive(),
			lderef(lindexer(lvar(["a"]), integer(0)))
		);
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_ref_path() {
		let (res, _): (AssignStatement, _) = build!(r#"*a::b::c = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lderef(lvar(["a", "b", "c"])));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_ref_ref() {
		let (res, _): (AssignStatement, _) = build!(r#"**a = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lderef(lderef(lvar(["a"]))));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_parenthesized() {
		let (res, _): (AssignStatement, _) = build!(r#"(a) = 5;"#, assign_statement);
		assert_eq!(res.lvalue.naive(), lvar(["a"]));
		assert_eq!(res.value.naive(), integer(5));
	}

	#[test]
	fn test_parenthesized_ref() {
		let (res, _): (AssignStatement, _) = build!(r#"*(a[0]) = 5;"#, assign_statement);
		assert_eq!(
			res.lvalue.naive(),
			lderef(lindexer(lvar(["a"]), integer(0)))
		);
		assert_eq!(res.value.naive(), integer(5));
	}
}
