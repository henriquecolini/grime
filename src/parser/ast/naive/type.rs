use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NaiveType {
	Simple(Vec<String>),
	Array(Box<NaiveType>, NaiveExpression),
	Slice(Box<NaiveType>),
	Ref(Box<NaiveType>),
	RefConst(Box<NaiveType>),
}

impl From<Type> for NaiveType {
	fn from(t: Type) -> NaiveType {
		match t {
			Type::Simple { name, .. } => NaiveType::Simple(name.naive()),
			Type::Array { ty, size, .. } => NaiveType::Array(Box::new(ty.naive()), size.into()),
			Type::Slice { ty, .. } => NaiveType::Slice(Box::new(ty.naive())),
			Type::Ref { ty, .. } => NaiveType::Ref(Box::new(ty.naive())),
			Type::RefConst { ty, .. } => NaiveType::RefConst(Box::new(ty.naive())),
		}
	}
}

pub fn tsimple<const N: usize>(path: [&str; N]) -> NaiveType {
	NaiveType::Simple(path.iter().map(|&s| s.into()).collect())
}

pub fn tarray(ty: NaiveType, size: NaiveExpression) -> NaiveType {
	NaiveType::Array(Box::new(ty), size)
}

pub fn tslice(ty: NaiveType) -> NaiveType {
	NaiveType::Slice(Box::new(ty))
}

pub fn tref(ty: NaiveType) -> NaiveType {
	NaiveType::Ref(Box::new(ty))
}

pub fn tref_const(ty: NaiveType) -> NaiveType {
	NaiveType::RefConst(Box::new(ty))
}
