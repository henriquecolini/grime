use super::*;

impl ResolvedModule {
	pub(super) fn register_declaration(&mut self, d: GlobalDeclaration) -> Result<(), Error> {
		match d {
			GlobalDeclaration::Use(_, m) => self.register_use(m),
			GlobalDeclaration::Module(_, m) => self.register_module(m),
			GlobalDeclaration::Variable(_, v) => self.register_variable(v),
			GlobalDeclaration::Function(_, f) => self.register_function(f),
			GlobalDeclaration::Struct(_, s) => self.register_struct(s),
			GlobalDeclaration::Alias(_, a) => self.register_alias(a),
		}
	}
	pub(super) fn register_use(&mut self, u: UseDeclaration) -> Result<(), Error> {
		let UseDeclaration { loc, name, glob } = u;
		let path = name.path;
		let res = self.search(&path).map_err(|e| {
			Error::new()
				.error(format!("unresolved import `{}`", path.join("::")))
				.with(e)
		})?;
		if glob {
			let Some(res) = res else {
				return Err(Error::new()
					.error(format!("unresolved import `{}`", path.join("::")))
					.location(loc.error(format!(
						"no `{}` in `{}`",
						path[path.len() - 1],
						path[..path.len() - 1].join("::")
					))));
			};
			let Item::Module(res) = res else {
				return Err(Error::new()
					.error(format!("expected module, found `{}`", path.last().unwrap())));
			};
			let mut new_mods = vec![];
			let mut new_aliases = vec![];
			let mut new_structs = vec![];
			let mut new_functions = vec![];
			let mut new_statics = vec![];
			let mut new_consts = vec![];
			for (k, v) in res.modules.iter_strong() {
				new_mods.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, v) in res.aliases.iter_strong() {
				new_aliases.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, v) in res.structs.iter_strong() {
				new_structs.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, v) in res.functions.iter_strong() {
				new_functions.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, v) in res.statics.iter_strong() {
				new_statics.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, v) in res.consts.iter_strong() {
				new_consts.push((k.to_owned(), loc.clone(), (*v).clone()))
			}
			for (k, l, v) in new_mods {
				self.modules
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			for (k, l, v) in new_aliases {
				self.aliases
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			for (k, l, v) in new_structs {
				self.structs
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			for (k, l, v) in new_functions {
				self.functions
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			for (k, l, v) in new_statics {
				self.statics
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			for (k, l, v) in new_consts {
				self.consts
					.weak_insert(k, l, v)
					.map_err(|l| Error::new().location(l.error("")))?;
			}
			Ok(())
		} else {
			let name = path.last().unwrap().to_owned();
			self.no_duplicates(&name, &loc)?;
			match res {
				Some(Item::Module(m)) => {
					self.modules.insert(name, loc, (*m).clone());
					Ok(())
				}
				Some(Item::Type(t)) => {
					self.aliases.insert(name, loc, (*t).clone());
					Ok(())
				}
				Some(Item::Struct(s)) => {
					self.structs.insert(name, loc, (*s).clone());
					Ok(())
				}
				Some(Item::Function(f)) => {
					self.functions.insert(name, loc, (*f).clone());
					Ok(())
				}
				Some(Item::Static(s)) => {
					self.statics.insert(name, loc, (*s).clone());
					Ok(())
				}
				Some(Item::Const(c)) => {
					self.consts.insert(name, loc, (*c).clone());
					Ok(())
				}
				None => Err(Error::new()
					.error(format!("unresolved import `{}`", path.join("::")))
					.location(loc.error(format!(
						"no `{}` in `{}`",
						path[path.len() - 1],
						path[..path.len() - 1].join("::")
					)))),
			}
		}
	}
	pub(super) fn register_module(&mut self, m: ModuleDeclaration) -> Result<(), Error> {
		use super::Error;
		use crate::parser::Error as ParseError;
		let loc = m.loc().clone();
		let name = m.name().name.clone();
		self.no_duplicates(&name, &loc)?;
		match Module::from_declaration(m) {
			Ok(m) => {
				let rm = ResolvedModule::resolve(m)?;
				self.modules.insert(name, loc, rm);
				Ok(())
			}
			Err(e) => Err(match e {
				ParseError::IO(io) => Error::new()
					.error(format!(
						"the module `{}` could not be opened due to an IO error",
						name
					))
					.error(format!("{}", io)),
				ParseError::Filename => Error::new().error(format!(
					"the module `{}` could not be opened because the filename is invalid",
					name
				)),
				ParseError::Parse(parse) => Error::new().location(parse),
			}),
		}
	}
	pub(super) fn register_variable(&mut self, v: GlobalVariableDeclaration) -> Result<(), Error> {
		match v {
			GlobalVariableDeclaration::Const {
				loc,
				name,
				ty,
				value,
			} => {
				self.no_duplicates(name.as_str(), &loc)?;
				self.consts.insert(name.name, loc, (ty, value));
			}
			GlobalVariableDeclaration::Static {
				loc,
				name,
				ty,
				value,
			} => {
				self.no_duplicates(name.as_str(), &loc)?;
				self.statics.insert(name.name, loc, (ty, value));
			}
		}
		Ok(())
	}

	pub(super) fn register_function(&mut self, f: FunctionDeclaration) -> Result<(), Error> {
		let FunctionDeclaration {
			loc,
			name,
			parameters,
			return_type,
			body,
		} = f;
		self.no_duplicates(name.as_ref(), &loc)?;
		self.functions
			.insert(name.name, loc, (parameters, return_type, body));
		Ok(())
	}

	pub(super) fn register_struct(&mut self, s: StructDeclaration) -> Result<(), Error> {
		let StructDeclaration { loc, name, fields } = s;
		self.no_duplicates(name.as_ref(), &loc)?;
		self.structs.insert(name.name, loc, fields);
		Ok(())
	}

	pub(super) fn register_alias(&mut self, a: AliasDeclaration) -> Result<(), Error> {
		let AliasDeclaration { loc, name, ty } = a;
		self.no_duplicates(name.as_ref(), &loc)?;
		self.aliases.insert(name.name, loc, ty);
		Ok(())
	}

	pub(super) fn no_duplicates(&self, name: &str, new: &Location) -> Result<(), Error> {
		let id = name;
		let fail = |prev: &Location| -> Error {
			Error::new()
				.with(format!("the name `{}` is defined multiple times", id))
				.with(prev.error(&format!("previous definition of `{}` here", id)))
				.with(new.error(&format!("`{}` redefined here", id)))
				.note("a name must be defined only once in a module")
		};
		self.modules
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))?;
		self.aliases
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))?;
		self.structs
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))?;
		self.functions
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))?;
		self.statics
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))?;
		self.consts
			.get_strong(name)
			.map_or(Ok(()), |n| Err(fail(&n.0)))
	}
}
