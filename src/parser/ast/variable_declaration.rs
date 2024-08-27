use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableDeclaration {
	Uninitialized {
		loc: Location,
		name: Identifier,
		ty: Option<Type>,
	},
	Initialized {
		loc: Location,
		name: Identifier,
		ty: Option<Type>,
		value: Expression,
	},
	Const {
		loc: Location,
		name: Identifier,
		ty: Type,
		value: Expression,
	},
	Static {
		loc: Location,
		name: Identifier,
		ty: Type,
		value: Expression,
	},
}

impl AstNode for VariableDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::var_decl);
		let child = parent.into_inner().expect_single();
		match child.as_rule() {
			Rule::simple_var_decl => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#let);
				let name = Identifier::build(ctx.clone(), children.expect_next());
				let ty = match children.next() {
					Some(child) => match child.as_rule() {
						Rule::colon => Some(Type::build(ctx.clone(), children.expect_next())),
						Rule::semicolon => None,
						_ => unreachable!(),
					},
					None => None,
				};
				VariableDeclaration::Uninitialized { loc, name, ty }
			}
			Rule::assign_var_decl => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#let);
				let name = Identifier::build(ctx.clone(), children.expect_next());
				let ty = match children.next() {
					Some(child) => match child.as_rule() {
						Rule::colon => {
							let ty = Type::build(ctx.clone(), children.expect_next());
							children.expect_next_rule(Rule::equals);
							Some(ty)
						}
						Rule::equals => None,
						_ => unreachable!(),
					},
					None => unreachable!(),
				};
				let value = Expression::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::semicolon);
				VariableDeclaration::Initialized {
					loc,
					name,
					ty,
					value,
				}
			}
			Rule::const_var_decl => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#const);
				let name = Identifier::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::colon);
				let ty = Type::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::equals);
				let value = Expression::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::semicolon);
				VariableDeclaration::Const {
					loc,
					name,
					ty,
					value,
				}
			}
			Rule::static_var_decl => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#static);
				let name = Identifier::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::colon);
				let ty = Type::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::equals);
				let value = Expression::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::semicolon);
				VariableDeclaration::Static {
					loc,
					name,
					ty,
					value,
				}
			}
			_ => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			VariableDeclaration::Uninitialized { loc, .. }
			| VariableDeclaration::Initialized { loc, .. }
			| VariableDeclaration::Const { loc, .. }
			| VariableDeclaration::Static { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			VariableDeclaration::Uninitialized { loc, .. }
			| VariableDeclaration::Initialized { loc, .. }
			| VariableDeclaration::Const { loc, .. }
			| VariableDeclaration::Static { loc, .. } => loc,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_uninitialized() {
		let (res, _): (VariableDeclaration, _) = build!(r#"let x;"#, var_decl);
		let VariableDeclaration::Uninitialized { name, ty, .. } = res else {
			panic!("Expected VariableDeclaration::Uninitialized, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty, None);
	}

	#[test]
	fn test_typed() {
		let (res, _): (VariableDeclaration, _) = build!(r#"let x: std::foo;"#, var_decl);
		let VariableDeclaration::Uninitialized { name, ty, .. } = res else {
			panic!("Expected VariableDeclaration::Uninitialized, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty.map(Naive::naive), Some(tsimple(["std", "foo"])));
	}

	#[test]
	fn test_initialized() {
		let (res, _): (VariableDeclaration, _) = build!(r#"let x = "hello";"#, var_decl);
		let VariableDeclaration::Initialized {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected VariableDeclaration::Initialized, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty, None);
		assert_eq!(value.naive(), string(b"hello"));
	}

	#[test]
	fn test_typed_init() {
		let (res, _): (VariableDeclaration, _) = build!(r#"let x: Foo = 5;"#, var_decl);
		let VariableDeclaration::Initialized {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected VariableDeclaration::Initialized, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty.map(Naive::naive), Some(tsimple(["Foo"])));
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_const() {
		let (res, _): (VariableDeclaration, _) = build!(r#"const x: Foo = 5;"#, var_decl);
		let VariableDeclaration::Const {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected VariableDeclaration::Const, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty.naive(), tsimple(["Foo"]));
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_static() {
		let (res, _): (VariableDeclaration, _) = build!(r#"static x: Foo = 5;"#, var_decl);
		let VariableDeclaration::Static {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected VariableDeclaration::Static, got {:?}", res);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty.naive(), tsimple(["Foo"]));
		assert_eq!(value.naive(), integer(5));
	}
}
