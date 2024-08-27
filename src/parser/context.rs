use super::Rule;
use std::{path::Path, rc::Rc};

use pest::{
	error::{Error, ErrorVariant},
	iterators::Pair,
	RuleType, Span,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
	path: Rc<Path>,
	input: Rc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Location {
	ctx: Context,
	start: usize,
	end: usize,
}

impl Context {
	pub fn new(path: &Path, input: &str) -> Self {
		Self {
			path: Rc::from(path),
			input: Rc::from(input),
		}
	}
	pub fn from_rc(path: Rc<Path>, input: Rc<str>) -> Self {
		Self { path, input }
	}
	pub fn nowhere() -> Self {
		let path: &Path = "".as_ref();
		Self {
			path: path.into(),
			input: "".into(),
		}
	}
	pub fn input(&self) -> &str {
		&self.input
	}
	pub fn paired(&self, pair: &Pair<'_, Rule>) -> Location {
		let span = pair.as_span();
		Location {
			ctx: self.clone(),
			start: span.start(),
			end: span.end(),
		}
	}
	pub fn as_location(&self) -> Location {
		Location {
			ctx: self.clone(),
			start: 0,
			end: self.input.len(),
		}
	}
}

impl Location {
	pub fn error<T: RuleType>(&self, message: impl AsRef<str>) -> Error<T> {
		let message = message.as_ref();
		Error::new_from_span(
			ErrorVariant::CustomError {
				message: message.into(),
			},
			Span::new(&self.ctx.input, self.start, self.end).unwrap(),
		)
		.with_path(self.ctx.path.to_string_lossy().as_ref())
	}
	pub fn path(&self) -> &Path {
		&self.ctx.path
	}
	pub fn nowhere() -> Self {
		Self {
			ctx: Context::nowhere(),
			start: 0,
			end: 0,
		}
	}
}

impl Default for Context {
	fn default() -> Self {
		Self {
			path: Rc::from(Path::new("")),
			input: Rc::from(""),
		}
	}
}
