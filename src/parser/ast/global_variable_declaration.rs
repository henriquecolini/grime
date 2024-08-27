use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalVariableDeclaration {
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

impl AstNode for GlobalVariableDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::global_var_decl);
		let child = parent.into_inner().expect_single();
		match child.as_rule() {
			Rule::const_var_decl => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#const);
				let name = Identifier::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::colon);
				let ty = Type::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::equals);
				let value = Expression::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::semicolon);
				GlobalVariableDeclaration::Const {
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
				GlobalVariableDeclaration::Static {
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
			GlobalVariableDeclaration::Const { loc, .. } => loc,
			GlobalVariableDeclaration::Static { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			GlobalVariableDeclaration::Const { loc, .. } => loc,
			GlobalVariableDeclaration::Static { loc, .. } => loc,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_const() {
		let (res, _): (GlobalVariableDeclaration, _) =
			build!(r#"const x: Foo = 5;"#, global_var_decl);
		let GlobalVariableDeclaration::Const {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected const, got {:?}", res);
		};
		assert_eq!(name.as_ref(), "x");
		assert_eq!(ty.naive(), tsimple(["Foo"]));
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_static() {
		let (res, _): (GlobalVariableDeclaration, _) =
			build!(r#"static x: Foo = 5;"#, global_var_decl);
		let GlobalVariableDeclaration::Static {
			name, ty, value, ..
		} = res
		else {
			panic!("Expected static, got {:?}", res);
		};
		assert_eq!(name.as_ref(), "x");
		assert_eq!(ty.naive(), tsimple(["Foo"]));
		assert_eq!(value.naive(), integer(5));
	}
}
