use std::fmt;

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub struct Identifier {
	pub loc: Location,
	pub name: String,
}

impl AstNode for Identifier {
	fn build(ctx: Context, entry: Pair<'_, Rule>) -> Self {
		entry.expect_rule(Rule::identifier);
		Identifier {
			loc: ctx.paired(&entry),
			name: entry.as_str().into(),
		}
	}
	fn loc(&self) -> &Location {
		&self.loc
	}
	fn loc_mut(&mut self) -> &mut Location {
		&mut self.loc
	}
}

impl Naive for Identifier {
	type Output = String;
	fn naive(self) -> Self::Output {
		self.name
	}
}

impl Identifier {
	pub fn as_str(&self) -> &str {
		&self.name
	}
}

impl fmt::Debug for Identifier {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl AsRef<str> for Identifier {
	fn as_ref(&self) -> &str {
		&self.name
	}
}
