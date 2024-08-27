use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnStatement {
	Unit { loc: Location },
	Value { loc: Location, value: Expression },
}

impl AstNode for ReturnStatement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::return_statement);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#return);
		match children.next() {
			Some(child) => match child.as_rule() {
				Rule::expression => ReturnStatement::Value {
					loc,
					value: Expression::build(ctx, child),
				},
				Rule::semicolon => ReturnStatement::Unit { loc },
				_ => unreachable!(),
			},
			None => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			ReturnStatement::Unit { loc } | ReturnStatement::Value { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			ReturnStatement::Unit { loc } | ReturnStatement::Value { loc, .. } => loc,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_unit() {
		let (res, _): (ReturnStatement, _) = build!(r#"return;"#, return_statement);
		let ReturnStatement::Unit { .. } = res else {
			panic!("Expected ReturnStatement::Unit, got {:?}", res);
		};
	}

	#[test]
	fn test_value() {
		let (res, _): (ReturnStatement, _) = build!(r#"return 5;"#, return_statement);
		let ReturnStatement::Value { value, .. } = res else {
			panic!("Expected ReturnStatement::Value, got {:?}", res);
		};
		assert_eq!(value.naive(), integer(5));
	}
}
