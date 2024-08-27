#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
	Not,
	Deref,
	Ref,
	RefConst,
}