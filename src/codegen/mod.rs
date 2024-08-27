// use std::collections::{HashMap, HashSet};

// use crate::parser::{
// 	ast::{
// 		BinaryOperator, Declaration, Identifier, Program, QualifiedName, Type, VariableDeclaration,
// 	},
// 	Module,
// };

// enum Code {
// 	BinaryOp {
// 		dst: Place,
// 		lhs: Place,
// 		op: BinaryOperator,
// 		rhs: Place,
// 	},
// 	Data {
// 		dst: Place,
// 		value: Constant,
// 	},
// }

// struct Instruction {
// 	uid: Uid,
// 	code: Code,
// }

// enum Place {
// 	Virtual(usize),
// 	Memory(MemoryPlace),
// }

// enum MemoryPlace {
// 	Variable(QualifiedName),
// 	Deref(Box<MemoryPlace>),
// 	Indexer {
// 		place: Box<MemoryPlace>,
// 		index: Box<Place>,
// 	},
// 	Accessor {
// 		place: Box<MemoryPlace>,
// 		name: Identifier,
// 	},
// }

// enum Constant {
// 	I8(i8),
// 	U8(u8),
// 	Pointer(u8),
// }

// struct Uid(u64);

// struct Procedure {
// 	instructions: Vec<Instruction>,
// }

// enum Typedef {
// 	Struct { fields: Vec<(QualifiedName, Type)> },
// 	Alias(Type),
// }

// struct Compilation {
// 	globals: HashMap<QualifiedName, Type>,
// 	types: HashMap<QualifiedName, Typedef>,
// 	constants: HashMap<QualifiedName, (Type, Constant)>,
// 	procedures: HashMap<QualifiedName, Procedure>,
// }

// impl Compilation {
// 	fn new() -> Self {
// 		Self {
// 			procedures: HashMap::new(),
// 			constants: HashMap::new(),
// 			globals: HashMap::new(),
// 			types: HashMap::new(),
// 		}
// 	}

// 	fn compile(&mut self, modules: Vec<Module>) {
// 		for module in modules {
// 			let data_name = QualifiedName(vec![module.name.clone(), "`data".into()]);
// 			self.add_procedure(
// 				&data_name,
// 				Procedure {
// 					instructions: Vec::new(),
// 				},
// 			);
// 			for declaration in module.prog.declarations {
// 				match declaration {
// 					Declaration::Variable(v) => self.compile_global(&module.name, &data_name, v),
// 					Declaration::Function(_) => todo!(),
// 					Declaration::Struct(_) => todo!(),
// 					Declaration::Alias(_) => todo!(),
// 				}
// 			}
// 		}
// 	}

// 	fn compile_global(
// 		&mut self,
// 		mod_name: &Identifier,
// 		data_name: &QualifiedName,
// 		variable: VariableDeclaration,
// 	) {
// 		match variable {
// 			VariableDeclaration::Uninitialized { name, ty } => {
// 				//self.globals.insert([mod_name, &name].into(), ty.clone());
// 			}
// 			VariableDeclaration::Initialized { name, ty, value } => todo!(),
// 			VariableDeclaration::Const { name, ty, value } => todo!(),
// 		}
// 	}

// 	fn add_procedure(&mut self, name: &QualifiedName, procedure: Procedure) {
// 		self.procedures.insert(name.clone(), procedure);
// 	}
// }
