use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NaiveLValue {
	Variable(Vec<String>),
	Accessor(Box<NaiveLValue>, String),
	Indexer(Box<NaiveLValue>, NaiveExpression),
	Deref(Box<NaiveLValue>),
}

impl From<LValue> for NaiveLValue {
	fn from(l: LValue) -> NaiveLValue {
		match l {
			LValue::Variable { name, .. } => {
				NaiveLValue::Variable(name.path.into_iter().map(|id| id).collect())
			}
			LValue::Accessor { lhs, rhs, .. } => {
				NaiveLValue::Accessor(Box::new((*lhs).into()), rhs.name)
			}
			LValue::Indexer { lhs, index, .. } => {
				NaiveLValue::Indexer(Box::new((*lhs).into()), index.into())
			}
			LValue::Deref { rhs, .. } => NaiveLValue::Deref(Box::new((*rhs).into())),
		}
	}
}

pub fn lvar<const N: usize>(path: [&str; N]) -> NaiveLValue {
	NaiveLValue::Variable(path.iter().map(|&s| s.into()).collect())
}

pub fn laccessor(base: NaiveLValue, field: &str) -> NaiveLValue {
	NaiveLValue::Accessor(Box::new(base), field.into())
}

pub fn lindexer(base: NaiveLValue, index: NaiveExpression) -> NaiveLValue {
	NaiveLValue::Indexer(Box::new(base), index)
}

pub fn lderef(base: NaiveLValue) -> NaiveLValue {
	NaiveLValue::Deref(Box::new(base))
}
