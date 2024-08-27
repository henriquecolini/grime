use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalDeclaration {
	Use(Location, UseDeclaration),
	Module(Location, ModuleDeclaration),
	Variable(Location, GlobalVariableDeclaration),
	Function(Location, FunctionDeclaration),
	Struct(Location, StructDeclaration),
	Alias(Location, AliasDeclaration),
}

impl AstNode for GlobalDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::global_declaration);
		let child = parent.into_inner().expect_single();
		match child.as_rule() {
			Rule::use_declaration => GlobalDeclaration::Use(loc, UseDeclaration::build(ctx, child)),
			Rule::module_declaration => {
				GlobalDeclaration::Module(loc, ModuleDeclaration::build(ctx, child))
			}
			Rule::global_var_decl => {
				GlobalDeclaration::Variable(loc, GlobalVariableDeclaration::build(ctx, child))
			}
			Rule::func_declaration => {
				GlobalDeclaration::Function(loc, FunctionDeclaration::build(ctx, child))
			}
			Rule::struct_declaration => {
				GlobalDeclaration::Struct(loc, StructDeclaration::build(ctx, child))
			}
			Rule::r#type_declaration => {
				GlobalDeclaration::Alias(loc, AliasDeclaration::build(ctx, child))
			}
			_ => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			GlobalDeclaration::Use(loc, _) => loc,
			GlobalDeclaration::Module(loc, _) => loc,
			GlobalDeclaration::Variable(loc, _) => loc,
			GlobalDeclaration::Function(loc, _) => loc,
			GlobalDeclaration::Struct(loc, _) => loc,
			GlobalDeclaration::Alias(loc, _) => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			GlobalDeclaration::Use(loc, _) => loc,
			GlobalDeclaration::Module(loc, _) => loc,
			GlobalDeclaration::Variable(loc, _) => loc,
			GlobalDeclaration::Function(loc, _) => loc,
			GlobalDeclaration::Struct(loc, _) => loc,
			GlobalDeclaration::Alias(loc, _) => loc,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_variable_declaration() {
		let (declaration, _): (GlobalDeclaration, _) =
			build!("const foo: i32 = 42;", global_declaration);
		let GlobalDeclaration::Variable(
			_,
			GlobalVariableDeclaration::Const {
				name, ty, value, ..
			},
		) = declaration
		else {
			panic!(
				"Expected GlobalDeclaration::Variable, got {:?}",
				declaration
			);
		};
		assert_eq!(name.name, "foo");
		assert_eq!(ty.naive(), tsimple(["i32"]));
		assert_eq!(value.naive(), integer(42));
	}

	#[test]
	fn test_function_declaration() {
		let (declaration, _): (GlobalDeclaration, _) =
			build!("fn foo(bar: i32) -> i32 {}", global_declaration);
		let GlobalDeclaration::Function(_, f) = declaration else {
			panic!(
				"Expected GlobalDeclaration::Function, got {:?}",
				declaration
			);
		};
		let [param] = f.parameters.to_array();
		assert_eq!(f.name.naive(), "foo");
		assert_eq!(param.name.naive(), "bar");
		assert_eq!(param.ty.naive(), tsimple(["i32"]));
		assert_eq!(f.return_type.map(Naive::naive), Some(tsimple(["i32"])));
	}

	#[test]
	fn test_struct_declaration() {
		let (declaration, _): (GlobalDeclaration, _) =
			build!("struct Foo { bar: i32 }", global_declaration);
		let GlobalDeclaration::Struct(_, s) = declaration else {
			panic!("Expected GlobalDeclaration::Struct, got {:?}", declaration);
		};
		let [field] = s.fields.to_array();
		assert_eq!(s.name.naive(), "Foo");
		assert_eq!(field.name.naive(), "bar");
		assert_eq!(field.ty.naive(), tsimple(["i32"]));
	}

	#[test]
	fn test_alias_declaration() {
		let (declaration, _): (GlobalDeclaration, _) =
			build!("type Foo = i32;", global_declaration);
		let GlobalDeclaration::Alias(_, a) = declaration else {
			panic!("Expected GlobalDeclaration::Alias, got {:?}", declaration);
		};
		assert_eq!(a.name.naive(), "Foo");
		assert_eq!(a.ty.naive(), tsimple(["i32"]));
	}

	#[test]
	fn test_use_declaration() {
		let (declaration, _): (GlobalDeclaration, _) = build!("use foo;", global_declaration);
		let GlobalDeclaration::Use(_, u) = declaration else {
			panic!("Expected GlobalDeclaration::Use, got {:?}", declaration);
		};
		assert_eq!(u.name.naive(), ["foo"]);
	}
}
