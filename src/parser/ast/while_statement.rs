use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileStatement {
	pub loc: Location,
	pub condition: Expression,
	pub body: Block,
}

impl AstNode for WhileStatement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#while);
		let condition = Expression::build(ctx.clone(), children.expect_next());
		let body = Block::build(ctx.clone(), children.expect_next());
		WhileStatement {
			loc,
			condition,
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
		let (res, _): (WhileStatement, _) = build!(r#"while true {}"#, while_statement);
		assert_eq!(res.condition.naive(), boolean(true));
		assert_eq!(res.body.statements, []);
	}
}
