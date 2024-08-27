use std::fmt;

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub struct Path {
	pub loc: Location,
	pub path: Vec<String>,
}

impl AstNode for Path {
	fn build(ctx: Context, entry: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&entry);
		entry.expect_rule(Rule::path);
		let identifiers = entry.into_inner().map(|p| p.as_str().into()).collect();
		Path {
			loc,
			path: identifiers,
		}
	}
	fn loc(&self) -> &Location {
		&self.loc
	}
	fn loc_mut(&mut self) -> &mut Location {
		&mut self.loc
	}
}

impl Naive for Path {
	type Output = Vec<String>;
	fn naive(self) -> Self::Output {
		self.path
	}
}

impl Path {
	pub fn child(&self, name: &str) -> Path {
		let mut new = self.clone();
		new.path.push(name.into());
		new
	}
}

impl fmt::Debug for Path {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}",
			self.path
				.iter()
				.map(|i| i.as_str())
				.collect::<Vec<_>>()
				.join("::")
		)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_one() {
		let (res, _): (Path, _) = build!(r#"Foo"#, path);
		assert_eq!(res.path, ["Foo"]);
	}

	#[test]
	fn test_three() {
		let (res, _): (Path, _) = build!(r#"A::B::C"#, path);
		assert_eq!(res.path, ["A", "B", "C"]);
	}
}
