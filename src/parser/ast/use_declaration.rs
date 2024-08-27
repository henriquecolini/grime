use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
	pub loc: Location,
	pub name: Path,
	pub glob: bool,
}

impl AstNode for UseDeclaration {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::use_declaration);
		let mut children = parent.into_inner();
		children.expect_next_rule(Rule::r#use);
		let name = Path::build(ctx, children.expect_next());
		let glob = children
			.next()
			.map_or(false, |pair| pair.as_rule() == Rule::asterisk);
		UseDeclaration { loc, name, glob }
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
	fn test_decl() {
		let (ret, _): (UseDeclaration, _) = build!("use foo::bar::baz;", use_declaration);
		let UseDeclaration { name, glob, .. } = ret;
		assert_eq!(name.path, ["foo", "bar", "baz"]);
		assert!(!glob);
	}

	#[test]
	fn test_decl_all() {
		let (ret, _): (UseDeclaration, _) = build!("use foo::bar::baz::*;", use_declaration);
		let UseDeclaration { name, glob, .. } = ret;
		assert_eq!(name.path, ["foo", "bar", "baz"]);
		assert!(glob);
	}
}
