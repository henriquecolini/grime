use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleDeclaration {
	Local {
		loc: Location,
		name: Identifier,
		declarations: Vec<GlobalDeclaration>,
	},
	File {
		loc: Location,
		name: Identifier,
	},
}

impl AstNode for ModuleDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		parent.expect_rule(Rule::module_declaration);
		let loc = ctx.paired(&parent);
		let mut pairs = parent.into_inner();
		pairs.expect_next_rule(Rule::r#mod);
		let name = Identifier::build(ctx.clone(), pairs.expect_next_rule(Rule::identifier));
		match pairs.expect_next().as_rule() {
			Rule::semicolon => {
				return ModuleDeclaration::File { loc, name };
			}
			Rule::lcurly => {
				let mut declarations = Vec::new();
				for pair in pairs {
					match pair.as_rule() {
						Rule::global_declaration => {
							declarations.push(AstNode::build(ctx.clone(), pair));
						}
						Rule::rcurly => break,
						_ => unreachable!(),
					}
				}
				ModuleDeclaration::Local {
					loc,
					name,
					declarations,
				}
			}
			_ => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			ModuleDeclaration::Local { loc, .. } => loc,
			ModuleDeclaration::File { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			ModuleDeclaration::Local { loc, .. } => loc,
			ModuleDeclaration::File { loc, .. } => loc,
		}
	}
}

impl ModuleDeclaration {
	pub fn name(&self) -> &Identifier {
		match self {
			ModuleDeclaration::Local { name, .. } | ModuleDeclaration::File { name, .. } => name,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_module_declaration() {
		let (declaration, _): (ModuleDeclaration, _) =
			build!("mod foo { const bar: i32 = 42; }", module_declaration);
		let ModuleDeclaration::Local {
			name, declarations, ..
		} = declaration
		else {
			panic!("Expected local module declaration");
		};
		let [GlobalDeclaration::Variable(
			_,
			GlobalVariableDeclaration::Const {
				name: vname,
				ty: vty,
				value: vvalue,
				..
			},
		)] = declarations.to_array()
		else {
			panic!("Expected variable declaration");
		};
		assert_eq!(name.naive(), "foo");
		assert_eq!(vname.naive(), "bar");
		assert_eq!(vty.naive(), tsimple(["i32"]));
		assert_eq!(vvalue.naive(), integer(42));
	}

	#[test]
	fn test_empty() {
		let (declaration, _): (ModuleDeclaration, _) = build!("mod foo;", module_declaration);
		let ModuleDeclaration::File { name, .. } = declaration else {
			panic!("Expected file module declaration");
		};
		assert_eq!(name.naive(), "foo");
	}

	#[test]
	fn test_file_module() {
		let (declaration, _): (ModuleDeclaration, _) = build!("mod foo;", module_declaration);
		let ModuleDeclaration::File { name, .. } = declaration else {
			panic!("Expected file module declaration");
		};
		assert_eq!(name.naive(), "foo");
	}
}
