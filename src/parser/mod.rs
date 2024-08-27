pub mod ast;
pub mod context;
mod error;

use std::{ffi::OsStr, path::Path, rc::Rc};

use ast::*;
use pest::{error::Error as ParseError, Parser};
use pest_derive::Parser;

#[derive(Parser, Debug)]
#[grammar = "parser/grammar.pest"]
pub struct GrimeParser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	pub name: String,
	pub path: Rc<Path>,
	pub prog: Ast,
}

#[derive(Debug)]
pub enum Error {
	IO(std::io::Error),
	Filename,
	Parse(ParseError<Rule>),
}

fn is_valid_identifier(s: &str) -> bool {
	let re = regex::Regex::new(r"^[a-zA-Z_][0-9a-zA-Z_]*$").unwrap();
	re.is_match(s)
}

fn parse_path(path: &Path) -> Result<String, Error> {
	if path.extension() != Some("gr".as_ref()) {
		return Err(Error::Filename);
	};
	match path.file_stem().and_then(OsStr::to_str) {
		Some(name) if is_valid_identifier(name) => Ok(name.into()),
		_ => Err(Error::Filename),
	}
}

impl Module {
	pub fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
		let path = path.as_ref();
		let name = parse_path(path)?;
		let input = std::fs::read_to_string(&path).map_err(Error::IO)?;
		let path: Rc<Path> = Rc::from(path);
		let input: Rc<str> = Rc::from(input);
		let context = Context::from_rc(path.clone(), input.clone());
		let pairs = GrimeParser::parse(Rule::program, input.as_ref())
			.map_err(|e| Error::Parse(error::prepare(&path.to_string_lossy(), e)))?;
		let prog = Ast::build(context, pairs);
		Ok(Module { name, path, prog })
	}
	pub fn from_declaration(decl: ModuleDeclaration) -> Result<Self, Error> {
		Ok(match decl {
			ModuleDeclaration::Local {
				loc,
				name,
				declarations,
			} => Module {
				path: {
					let mut path = loc.path().with_extension("");
					path.push(name.as_ref());
					path.set_extension("gr");
					Rc::from(path)
				},
				name: name.name,
				prog: Ast { declarations },
			},
			ModuleDeclaration::File { loc, name } => {
				let mut path = loc.path().with_extension("");
				path.push(name.as_ref());
				path.set_extension("gr");
				Module::open(path)?
			}
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_path() {
		let id = parse_path("foo/bar/baz.gr".as_ref()).expect("parse path");
		assert_eq!(id, "baz");

		let id = parse_path("baz.gr".as_ref()).expect("parse path");
		assert_eq!(id, "baz");

		let id = parse_path("/foo/bar/baz.gr".as_ref()).expect("parse path");
		assert_eq!(id, "baz");

		assert!(parse_path("foo/bar/baz".as_ref()).is_err());
		assert!(parse_path("foo/bar/baz.g".as_ref()).is_err());
		assert!(parse_path("foo/bar/baz.grs".as_ref()).is_err());
		assert!(parse_path("foo/bar/baz.rs".as_ref()).is_err());
		assert!(parse_path("foo/bar/~baz.gr".as_ref()).is_err());
		assert!(parse_path("foo/bar/0baz.gr".as_ref()).is_err());
		assert!(parse_path("foo/bar/baz .gr".as_ref()).is_err());
		assert!(parse_path("foo/bar/baz.gr ".as_ref()).is_err());
		assert!(parse_path(" foo/bar/baz".as_ref()).is_err());
	}

	#[test]
	fn test_open() {
		let module = Module::open("test/hello.gr").expect("open module");
		assert_eq!(module.name, "hello");
	}

	#[test]
	fn test_from_declaration() {
		let module = Module::open("test/mods.gr").expect("open module");
		let decl = module.prog.declarations.first().unwrap().clone();
		let GlobalDeclaration::Module(_, decl) = decl else {
			panic!("Expected GlobalDeclaration::Module, got {:?}", decl);
		};
		let module = Module::from_declaration(decl).expect("from declaration");
		assert_eq!(module.name, "foo");
		assert_eq!(module.path.to_string_lossy(), "test/mods/foo.gr");
		let module = module.prog.declarations.first().unwrap().clone();
		let GlobalDeclaration::Module(_, decl) = module else {
			panic!("Expected GlobalDeclaration::Module, got {:?}", module);
		};
		Module::from_declaration(decl).expect_err("from declaration");
	}
}
