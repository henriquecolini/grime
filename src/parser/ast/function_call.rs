use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	pub loc: Location,
	pub name: Path,
	pub args: Vec<Expression>,
}

impl AstNode for FunctionCall {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::func_call);
		let mut children = parent.into_inner();
		let name = Path::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::lparent);
		let mut args = vec![];
		for child in children {
			if child.as_rule() == Rule::rparent {
				break;
			}
			if child.as_rule() == Rule::comma {
				continue;
			}
			args.push(Expression::build(ctx.clone(), child));
		}
		Self { loc, name, args }
	}
	fn loc(&self) -> &Location {
		&self.loc
	}
	fn loc_mut(&mut self) -> &mut Location {
		&mut self.loc
	}
}

impl FunctionCall {
	pub fn naive(self) -> NaiveFunctionCall {
		NaiveFunctionCall {
			name: self.name.path,
			args: self.args.into_iter().map(NaiveExpression::from).collect(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_empty() {
		let (res, _): (FunctionCall, _) = build!(r#"foo()"#, func_call);
		let res = res.naive();
		assert_eq!(res.name, ["foo"]);
		assert_eq!(res.args, vec![]);
	}

	#[test]
	fn test_two() {
		let (res, _): (FunctionCall, _) = build!(r#"foo(bar, 42)"#, func_call);
		let res = res.naive();
		assert_eq!(res.name, ["foo"]);
		assert_eq!(res.args, vec![var(["bar"]), integer(42)]);
	}

	#[test]
	fn test_path() {
		let (res, _): (FunctionCall, _) = build!(r#"foo::bar()"#, func_call);
		let res = res.naive();
		assert_eq!(res.name, ["foo", "bar"]);
		assert_eq!(res.args, vec![]);
	}
}
