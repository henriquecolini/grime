use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub loc: Location,
	pub statements: Vec<Statement>,
}

impl AstNode for Block {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::block);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::lcurly);
		let mut statements = vec![];
		for child in children {
			match child.as_rule() {
				Rule::statement => statements.push(Statement::build(ctx.clone(), child)),
				Rule::rcurly => break,
				_ => unreachable!(),
			}
		}
		Block { loc, statements }
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
	fn test_empty() {
		let (res, _): (Block, _) = build!(r#"{}"#, block);
		assert_eq!(res.statements.len(), 0);
	}

	#[test]
	fn test_one() {
		let (res, _): (Block, _) = build!(r#"{ return; }"#, block);
		assert_eq!(res.statements.len(), 1);
	}

	#[test]
	fn test_three() {
		let (res, _): (Block, _) = build!(r#"{ return; let x = 5; x = 6; }"#, block);
		assert_eq!(res.statements.len(), 3);
	}
}
