use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
	pub loc: Location,
	pub name: Identifier,
	pub parameters: Vec<Parameter>,
	pub return_type: Option<Type>,
	pub body: Block,
}

impl AstNode for FunctionDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::func_declaration);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#fn);
		let name = Identifier::build(ctx.clone(), children.expect_next());
		children.expect_next_rule(Rule::lparent);
		let parameters = if children.peek().map(|c| c.as_rule()) == Some(Rule::parameter_list) {
			children
				.expect_next()
				.into_inner()
				.filter(|pair| pair.as_rule() == Rule::parameter)
				.map(|pair| Parameter::build(ctx.clone(), pair))
				.collect()
		} else {
			vec![]
		};
		children.expect_next_rule(Rule::rparent);
		let return_type = if children.peek().map(|c| c.as_rule()) == Some(Rule::rarrow) {
			children.next();
			Some(Type::build(ctx.clone(), children.expect_next()))
		} else {
			None
		};
		let body = Block::build(ctx, children.expect_next());
		FunctionDeclaration {
			loc,
			name,
			parameters,
			return_type,
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
	fn test_empty() {
		let (declaration, _): (FunctionDeclaration, _) = build!("fn foo() {}", func_declaration);
		assert_eq!(declaration.name.name, "foo");
		assert_eq!(declaration.parameters, vec![]);
		assert_eq!(declaration.return_type, None);
		assert_eq!(declaration.body.statements, vec![]);
	}

	#[test]
	fn test_one() {
		let (declaration, _): (FunctionDeclaration, _) =
			build!("fn foo(bar: i32) {}", func_declaration);
		let [Parameter {
			name: pname,
			ty: ptype,
			..
		}] = declaration.parameters.to_array();
		assert_eq!(declaration.name.name, "foo");
		assert_eq!(pname.naive(), "bar");
		assert_eq!(ptype.naive(), tsimple(["i32"]));
		assert_eq!(declaration.return_type, None);
		assert_eq!(declaration.body.statements, vec![]);
	}

	#[test]
	fn test_two() {
		let (declaration, _): (FunctionDeclaration, _) =
			build!("fn foo(bar: i32, baz: u8) {}", func_declaration);
		let [Parameter {
			name: pname,
			ty: ptype,
			..
		}, Parameter {
			name: pname2,
			ty: ptype2,
			..
		}] = declaration.parameters.to_array();
		assert_eq!(declaration.name.naive(), "foo");
		assert_eq!(pname.naive(), "bar");
		assert_eq!(ptype.naive(), tsimple(["i32"]));
		assert_eq!(pname2.naive(), "baz");
		assert_eq!(ptype2.naive(), tsimple(["u8"]));
		assert_eq!(declaration.return_type, None);
		assert_eq!(declaration.body.statements, vec![]);
	}

	#[test]
	fn test_return() {
		let (declaration, _): (FunctionDeclaration, _) =
			build!("fn foo() -> i32 {}", func_declaration);
		assert_eq!(declaration.name.naive(), "foo");
		assert_eq!(declaration.parameters, vec![]);
		assert_eq!(
			declaration.return_type.map(|t| t.naive()),
			Some(tsimple(["i32"]))
		);
	}

	#[test]
	fn test_one_return() {
		let (declaration, _): (FunctionDeclaration, _) =
			build!("fn foo(bar: i32) -> u8 {}", func_declaration);
		let [Parameter {
			name: pname,
			ty: ptype,
			..
		}] = declaration.parameters.to_array();
		assert_eq!(declaration.name.naive(), "foo");
		assert_eq!(pname.naive(), "bar");
		assert_eq!(ptype.naive(), tsimple(["i32"]));
		assert_eq!(
			declaration.return_type.map(|t| t.naive()),
			Some(tsimple(["u8"]))
		);
	}
}
