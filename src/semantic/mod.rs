// 1. Resolve symbols.
//   - Recursively resolve identifiers and qualified paths
//   - Look for all types and functions
//   - Look for global variables and constants
//   - Look for local variables in scope
//   - Ensure no duplicates
//   - Ensure a single main function
//   INPUT: AST
//   OUTPUT: Resolved AST

// 2. Type checking.
//   - Infer types for expressions
//   - Check types for assignments and function calls
//   - Ensure correct types for return statements
//   - Ensure correct types within expressions (arrays, operations)
//   - Ensure constant expressions are constant
//   INPUT: Resolved AST
//   OUTPUT: Typed AST

// 3. Intermediate representation.
//   - Break down expressions into simplest forms
//   - Replace loops with gotos and branches
//   - Delete unreachable code and unused symbols and functions
//   INPUT: Typed AST
//   OUTPUT: IR

// 4. Optimization.
//   - Inline functions
//   - Remove dead code (unused assignments, unused variables)
//   - Minimize memory usage (reuse registers, reuse memory)
//   - Minimize instruction count (replace with simpler instructions)
//   - Minimize branch count (replace with conditional moves)
//   INPUT: IR
//   OUTPUT: Optimized IR

// 5. Code generation.
//   - Generate assembly code for each function
//   - Generate data sections for global data
//   - Create labels
//   - Create jump instructions
//   INPUT: Optimized IR
//   OUTPUT: Assembly code

// 6. Assembly.
//   - Assemble the assembly code into object/binary files
//   INPUT: Assembly code
//   OUTPUT: Binary

use crate::parser::Rule;

pub mod resolver;

pub enum ErrorSegment {
	Error(String),
	Warning(String),
	Note(String),
	Help(String),
	Location(pest::error::Error<Rule>),
}

pub struct Error(Vec<ErrorSegment>);

impl From<&str> for Error {
	fn from(s: &str) -> Self {
		Error(vec![ErrorSegment::Error(s.into())])
	}
}

impl From<String> for Error {
	fn from(s: String) -> Self {
		Error(vec![ErrorSegment::Error(s)])
	}
}

impl From<pest::error::Error<Rule>> for Error {
	fn from(e: pest::error::Error<Rule>) -> Self {
		Error(vec![ErrorSegment::Location(e)])
	}
}

impl From<ErrorSegment> for Error {
	fn from(segment: ErrorSegment) -> Self {
		Error(vec![segment])
	}
}

impl Error {
	pub fn new() -> Self {
		Error(vec![])
	}
	pub fn with(self, segment: impl Into<Error>) -> Self {
		let mut v = self.0;
		v.extend(segment.into().0);
		Error(v)
	}
	pub fn error(self, s: impl AsRef<str>) -> Self {
		self.with(ErrorSegment::Error(s.as_ref().into()))
	}
	pub fn warning(self, s: impl AsRef<str>) -> Self {
		self.with(ErrorSegment::Warning(s.as_ref().into()))
	}
	pub fn note(self, s: impl AsRef<str>) -> Self {
		self.with(ErrorSegment::Note(s.as_ref().into()))
	}
	pub fn help(self, s: impl AsRef<str>) -> Self {
		self.with(ErrorSegment::Help(s.as_ref().into()))
	}
	pub fn location(self, e: pest::error::Error<Rule>) -> Self {
		self.with(ErrorSegment::Location(e))
	}
}

impl std::fmt::Display for ErrorSegment {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ErrorSegment::Error(s) => write!(f, "error: {}", s),
			ErrorSegment::Warning(s) => write!(f, "warning: {}", s),
			ErrorSegment::Note(s) => write!(f, "note: {}", s),
			ErrorSegment::Help(s) => write!(f, "help: {}", s),
			ErrorSegment::Location(e) => write!(f, "{}", e),
		}
	}
}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut first = true;
		for segment in &self.0 {
			if !first {
				writeln!(f)?;
			}
			write!(f, "{}", segment)?;
			first = false;
		}
		Ok(())
	}
}
