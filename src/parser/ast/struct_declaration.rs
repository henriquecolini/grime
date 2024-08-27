use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeclaration {
	pub loc: Location,
	pub name: Identifier,
	pub fields: Vec<Field>,
}

impl AstNode for StructDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::struct_declaration);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#struct);
		let name = Identifier::build(ctx.clone(), children.expect_next());
		let fields = children
			.expect_single()
			.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				Rule::field => Some(Field::build(ctx.clone(), pair)),
				_ => None,
			})
			.collect();
		StructDeclaration { loc, name, fields }
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
		let (declaration, _): (StructDeclaration, _) = build!("struct Foo {}", struct_declaration);
		let StructDeclaration { name, fields, .. } = declaration;
		assert_eq!(name.naive(), "Foo");
		assert_eq!(fields, []);
	}

	#[test]
	fn test_fields() {
		let (declaration, _): (StructDeclaration, _) =
			build!("struct Foo { foo: i32, bar: f64 }", struct_declaration);
		let StructDeclaration { name, fields, .. } = declaration;
		let [f1, f2] = fields.to_array();
		assert_eq!(name.naive(), "Foo");
		assert_eq!(f1.name.naive(), "foo");
		assert_eq!(f1.ty.naive(), tsimple(["i32"]));
		assert_eq!(f2.name.naive(), "bar");
		assert_eq!(f2.ty.naive(), tsimple(["f64"]));
	}

	#[test]
	fn test_path_fields() {
		let (declaration, _): (StructDeclaration, _) = build!(
			"struct Foo { foo: bar::Baz, bar: baz::Qux }",
			struct_declaration
		);
		let StructDeclaration { name, fields, .. } = declaration;
		let [f1, f2] = fields.to_array();
		assert_eq!(name.naive(), "Foo");
		assert_eq!(f1.name.naive(), "foo");
		assert_eq!(f1.ty.naive(), tsimple(["bar", "Baz"]));
		assert_eq!(f2.name.naive(), "bar");
		assert_eq!(f2.ty.naive(), tsimple(["baz", "Qux"]));
	}
}
