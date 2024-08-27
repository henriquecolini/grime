use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub declarations: Vec<GlobalDeclaration>,
}

impl Ast {
	pub fn build(ctx: Context, pairs: Pairs<'_, Rule>) -> Self {
		let mut declarations = Vec::new();
		for pair in pairs {
			match pair.as_rule() {
				Rule::global_declaration => {
					declarations.push(AstNode::build(ctx.clone(), pair));
				}
				Rule::EOI => {}
				_ => unreachable!(),
			}
		}
		Ast { declarations }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_program() {
		use GlobalDeclaration::*;
		let ctx = Context::new(
			"".as_ref(),
			"use std;
			fn main() {}
			mod foo;
			const bar: i8 = 42;",
		);
		let ctx1 = ctx.clone();
		let pairs = GrimeParser::parse(Rule::program, ctx.input()).expect("Failed to parse");
		let program = Ast::build(ctx1, pairs);
		let [Use(..), Function(..), Module(..), Variable(..)] = program.declarations.to_array()
		else {
			panic!("Expected use and function declarations");
		};
	}
}
