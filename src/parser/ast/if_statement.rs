use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
	pub loc: Location,
	pub condition: Expression,
	pub then_block: Block,
	pub else_block: Option<Block>,
}

impl AstNode for IfStatement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		match parent.as_rule() {
			Rule::if_statement => {
				let mut children = parent.into_inner();
				children.expect_next_rule(Rule::r#if);
				let condition = Expression::build(ctx.clone(), children.expect_next());
				let then_block = Block::build(ctx, children.expect_next());
				IfStatement {
					loc,
					condition,
					then_block,
					else_block: None,
				}
			}
			Rule::if_else_statement => {
				let mut children = parent.into_inner();
				children.expect_next_rule(Rule::r#if);
				let condition = Expression::build(ctx.clone(), children.expect_next());
				let then_block = Block::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::r#else);
				let else_block = Block::build(ctx, children.expect_next());
				IfStatement {
					loc,
					condition,
					then_block,
					else_block: Some(else_block),
				}
			}
			_ => unreachable!(),
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
	fn test_if_statement() {
		let (statement, _): (IfStatement, _) = build!("if true {}", if_statement);
		let IfStatement {
			condition,
			then_block,
			else_block,
			..
		} = statement;
		assert_eq!(condition.naive(), boolean(true));
		assert_eq!(then_block.statements, []);
		assert_eq!(else_block, None);
	}

	#[test]
	fn test_if_else_statement() {
		let (statement, _): (IfStatement, _) = build!("if true {} else {}", if_else_statement);
		let IfStatement {
			condition,
			then_block,
			else_block,
			..
		} = statement;
		assert_eq!(condition.naive(), boolean(true));
		assert_eq!(then_block.statements, []);
		assert_eq!(else_block.map(|b| b.statements), Some(vec![]));
	}
}
