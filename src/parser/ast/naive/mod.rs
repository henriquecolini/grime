pub mod expression;
pub mod lvalue;
pub mod r#type;

pub use super::*;

pub use expression::*;
pub use lvalue::*;
pub use r#type::*;

pub trait Naive {
	type Output;
	fn naive(self) -> Self::Output;
}
