use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForStatement {
	pub loc: Location,
	pub variable: LValue,
	pub iterable: Expression,
	pub body: Block,
}

impl AstNode for ForStatement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::for_statement);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#for);
		let variable = LValue::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::r#in);
		let iterable = Expression::build(ctx.clone(), children.expect_next());
		let body = Block::build(ctx.clone(), children.expect_next());
		ForStatement {
			loc,
			variable,
			iterable,
			body,
		}
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
		let (res, _): (ForStatement, _) = build!(r#"for x in [1, 2, 3] {}"#, for_statement);
		let ForStatement {
			variable,
			iterable,
			body,
			..
		} = res;
		assert_eq!(variable.naive(), lvar(["x"]));
		assert_eq!(
			iterable.naive(),
			array([integer(1), integer(2), integer(3)])
		);
		assert_eq!(body.statements, []);
	}
}
