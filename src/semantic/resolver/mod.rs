// 1. Resolve symbols.
//   - Recursively resolve identifiers and qualified paths
//   - Look for all types and functions
//   - Look for global variables and constants
//   - Look for local variables in scope
//   - Ensure no duplicates
//   - Ensure a single main function
//   INPUT: AST
//   OUTPUT: Resolved AST

#![allow(dead_code)]

mod item_list;
mod register;

use item_list::ItemList;

use super::Error;
use crate::parser::{ast::*, Module};
use std::{path::Path, rc::Rc};

#[derive(Debug, Clone)]
pub enum Item<'a> {
	Module(&'a ResolvedModule),
	Type(&'a Type),
	Struct(&'a Vec<Field>),
	Function(&'a (Vec<Parameter>, Option<Type>, Block)),
	Static(&'a (Type, Expression)),
	Const(&'a (Type, Expression)),
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
	pub name: String,
	pub path: Rc<Path>,
	pub modules: ItemList<ResolvedModule>,
	pub aliases: ItemList<Type>,
	pub structs: ItemList<Vec<Field>>,
	pub functions: ItemList<(Vec<Parameter>, Option<Type>, Block)>,
	pub statics: ItemList<(Type, Expression)>,
	pub consts: ItemList<(Type, Expression)>,
}

impl ResolvedModule {
	fn new(name: String, path: Rc<Path>) -> Self {
		Self {
			name,
			path,
			modules: ItemList::new(),
			aliases: ItemList::new(),
			structs: ItemList::new(),
			functions: ItemList::new(),
			statics: ItemList::new(),
			consts: ItemList::new(),
		}
	}

	pub fn resolve(mo: Module) -> Result<ResolvedModule, Error> {
		let mut rm = ResolvedModule::new(mo.name, mo.path);
		for d in mo.prog.declarations {
			rm.register_declaration(d)?;
		}
		// println!("{:?}", rm.aliases.keys());
		// println!("{:?}", rm.structs.keys());
		// println!("{:?}", rm.functions.keys());
		// println!("{:?}", rm.statics.keys());
		// println!("{:?}", rm.consts.keys());
		Ok(rm)
	}

	pub fn search_one(&self, name: &str) -> Option<Item<'_>> {
		if let Some(it) = self.aliases.get(name) {
			Some(Item::Type(it))
		} else if let Some(it) = self.structs.get(name) {
			Some(Item::Struct(it))
		} else if let Some(it) = self.functions.get(name) {
			Some(Item::Function(it))
		} else if let Some(it) = self.statics.get(name) {
			Some(Item::Static(it))
		} else if let Some(it) = self.consts.get(name) {
			Some(Item::Const(it))
		} else if let Some(it) = self.modules.get(name) {
			Some(Item::Module(it))
		} else {
			None
		}
	}

	pub fn search(&self, path: &[String]) -> Result<Option<Item<'_>>, Error> {
		match path {
			[name] => Ok(self.search_one(name)),
			[name, rest @ ..] => match self.search_one(name) {
				Some(Item::Module(m)) => m.search(rest),
				Some(Item::Type(_)) => Err(Error::new().error(format!(
					"expected module, found `{}` in type `{}`",
					rest[0], name
				))),
				Some(Item::Struct(_)) => Err(Error::new().error(format!(
					"expected module, found `{}` in struct `{}`",
					rest[0], name
				))),
				Some(Item::Function(_)) => Err(Error::new().error(format!(
					"expected module, found `{}` in function `{}`",
					rest[0], name
				))),
				Some(Item::Static(_)) => Err(Error::new().error(format!(
					"expected module, found `{}` in static variable `{}`",
					rest[0], name
				))),
				Some(Item::Const(_)) => Err(Error::new().error(format!(
					"expected module, found `{}` in constant `{}`",
					rest[0], name
				))),
				None => Ok(None),
			},
			[] => unreachable!(),
		}
	}
}

trait Qualify {
	fn qualify(&self) -> Vec<String>;
}

impl Qualify for String {
	fn qualify(&self) -> Vec<String> {
		vec![self.clone()]
	}
}

impl Qualify for Vec<String> {
	fn qualify(&self) -> Vec<String> {
		self.clone()
	}
}

impl<Q: Qualify> Qualify for &Q {
	fn qualify(&self) -> Vec<String> {
		(*self).qualify()
	}
}

fn child(parent: &impl Qualify, child: &str) -> Vec<String> {
	let mut p = parent.qualify();
	p.push(child.into());
	p
}
