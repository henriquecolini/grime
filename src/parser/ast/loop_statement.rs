use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopStatement {
	pub loc: Location,
	pub body: Block,
}

impl AstNode for LoopStatement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::loop_statement);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#loop);
		let body = Block::build(ctx, children.expect_next());
		LoopStatement { loc, body }
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
		let (res, _): (LoopStatement, _) = build!(r#"loop {}"#, loop_statement);
		let LoopStatement { body, .. } = res;
		assert_eq!(body.statements, []);
	}
}
