use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NaiveExpression {
	Integer(i64),
	String(Vec<u8>),
	Boolean(bool),
	Array(Vec<NaiveExpression>),
	FunctionCall(NaiveFunctionCall),
	Variable(Vec<String>),
	BinaryOp {
		op: BinaryOperator,
		left: Box<NaiveExpression>,
		right: Box<NaiveExpression>,
	},
	UnaryOp {
		op: UnaryOperator,
		expr: Box<NaiveExpression>,
	},
	Accessor {
		base: Box<NaiveExpression>,
		field: String,
	},
	Slicer {
		base: Box<NaiveExpression>,
		from: Option<Box<NaiveExpression>>,
		until: Option<Box<NaiveExpression>>,
	},
	Indexer {
		base: Box<NaiveExpression>,
		index: Box<NaiveExpression>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NaiveFunctionCall {
	pub name: Vec<String>,
	pub args: Vec<NaiveExpression>,
}

impl From<Expression> for NaiveExpression {
	fn from(e: Expression) -> NaiveExpression {
		match e {
			Expression::Integer { value, .. } => NaiveExpression::Integer(value),
			Expression::String { value, .. } => NaiveExpression::String(value),
			Expression::Boolean { value, .. } => NaiveExpression::Boolean(value),
			Expression::Array { value, .. } => {
				NaiveExpression::Array(value.into_iter().map(|e| e.into()).collect())
			}
			Expression::FunctionCall { value, .. } => NaiveExpression::FunctionCall(value.naive()),
			Expression::Variable { value, .. } => NaiveExpression::Variable(value.naive()),
			Expression::BinaryOp {
				op, left, right, ..
			} => NaiveExpression::BinaryOp {
				op,
				left: Box::new((*left).into()),
				right: Box::new((*right).into()),
			},
			Expression::UnaryOp { op, expr, .. } => NaiveExpression::UnaryOp {
				op,
				expr: Box::new((*expr).into()),
			},
			Expression::Accessor { base, field, .. } => NaiveExpression::Accessor {
				base: Box::new((*base).into()),
				field: field.name,
			},
			Expression::Slicer {
				base, from, until, ..
			} => NaiveExpression::Slicer {
				base: Box::new((*base).into()),
				from: from.map(|e| Box::new((*e).into())),
				until: until.map(|e| Box::new((*e).into())),
			},
			Expression::Indexer { base, index, .. } => NaiveExpression::Indexer {
				base: Box::new((*base).into()),
				index: Box::new((*index).into()),
			},
		}
	}
}

impl From<FunctionCall> for NaiveFunctionCall {
	fn from(call: FunctionCall) -> Self {
		Self {
			name: call.name.path.into_iter().map(|i| i.into()).collect(),
			args: call.args.into_iter().map(NaiveExpression::from).collect(),
		}
	}
}

pub fn integer(value: i64) -> NaiveExpression {
	NaiveExpression::Integer(value)
}
pub fn string(value: &[u8]) -> NaiveExpression {
	NaiveExpression::String(value.to_owned())
}
pub fn boolean(value: bool) -> NaiveExpression {
	NaiveExpression::Boolean(value)
}
pub fn array<const N: usize>(value: [NaiveExpression; N]) -> NaiveExpression {
	NaiveExpression::Array(value.into())
}
pub fn call<const N: usize, const K: usize>(
	name: [&str; N],
	args: [NaiveExpression; K],
) -> NaiveExpression {
	NaiveExpression::FunctionCall(NaiveFunctionCall {
		name: name.into_iter().map(|i| i.into()).collect(),
		args: args.into(),
	})
}
pub fn var<const N: usize>(value: [&str; N]) -> NaiveExpression {
	NaiveExpression::Variable(value.into_iter().map(|i| i.into()).collect())
}
pub fn binary(
	op: BinaryOperator,
	left: NaiveExpression,
	right: NaiveExpression,
) -> NaiveExpression {
	NaiveExpression::BinaryOp {
		op,
		left: Box::new(left),
		right: Box::new(right),
	}
}
pub fn unary(op: UnaryOperator, expr: NaiveExpression) -> NaiveExpression {
	NaiveExpression::UnaryOp {
		op,
		expr: Box::new(expr),
	}
}
pub fn accessor(base: NaiveExpression, field: &str) -> NaiveExpression {
	NaiveExpression::Accessor {
		base: Box::new(base),
		field: field.into(),
	}
}
pub fn slicer(
	base: NaiveExpression,
	from: Option<NaiveExpression>,
	until: Option<NaiveExpression>,
) -> NaiveExpression {
	NaiveExpression::Slicer {
		base: Box::new(base),
		from: from.map(Box::new),
		until: until.map(Box::new),
	}
}
pub fn indexer(base: NaiveExpression, index: NaiveExpression) -> NaiveExpression {
	NaiveExpression::Indexer {
		base: Box::new(base),
		index: Box::new(index),
	}
}
