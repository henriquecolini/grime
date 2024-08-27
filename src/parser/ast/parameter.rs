use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
	pub loc: Location,
	pub name: Identifier,
	pub ty: Type,
}

impl AstNode for Parameter {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::parameter);
		let mut children = parent.into_inner();
		let name = Identifier::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::colon);
		let ty = Type::build(ctx.clone(), children.expect_next());
		Parameter { loc, name, ty }
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
	fn test_parameter() {
		let (parameter, _): (Parameter, _) = build!("foo: i32", parameter);
		assert_eq!(parameter.name.naive(), "foo");
		assert_eq!(parameter.ty.naive(), tsimple(["i32"]));
	}
}
