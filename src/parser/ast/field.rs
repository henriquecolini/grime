use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
	pub loc: Location,
	pub name: Identifier,
	pub ty: Type,
}

impl AstNode for Field {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::field);
		let mut children = parent.into_inner();
		let name = Identifier::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::colon);
		let ty = Type::build(ctx, children.expect_next());
		Field { loc, name, ty }
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
	fn test_field() {
		let (field, _): (Field, _) = build!("foo: i32", field);
		assert_eq!(field.name.as_ref(), "foo");
		assert_eq!(field.ty.naive(), tsimple(["i32"]));
	}
}
