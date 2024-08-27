use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasDeclaration {
	pub loc: Location,
	pub name: Identifier,
	pub ty: Type,
}

impl AstNode for AliasDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::type_declaration);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#type);
		let name = Identifier::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::equals);
		let ty = Type::build(ctx, children.expect_next());
		children.expect_next_rule(Rule::semicolon);
		AliasDeclaration { loc, name, ty }
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
		let (res, _): (AliasDeclaration, _) = build!(r#"type Foo = A;"#, type_declaration);
		assert_eq!(res.name.naive(), "Foo");
		assert_eq!(res.ty.naive(), tsimple(["A"]));
	}

	#[test]
	fn test_path() {
		let (res, _): (AliasDeclaration, _) = build!(r#"type Foo = A::B::C;"#, type_declaration);
		assert_eq!(res.name.naive(), "Foo");
		assert_eq!(res.ty.naive(), tsimple(["A", "B", "C"]));
	}

	#[test]
	fn test_complex_type() {
		let (res, _): (AliasDeclaration, _) =
			build!(r#"type Foo = &const [&[u8]];"#, type_declaration);
		assert_eq!(res.name.as_ref(), "Foo");
		assert_eq!(
			res.ty.naive(),
			tref_const(tslice(tref(tslice(tsimple(["u8"])))))
		);
	}
}
