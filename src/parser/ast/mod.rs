pub mod alias_declaration;
pub mod assignment_statement;
pub mod ast;
pub mod binary_operator;
pub mod block;
pub mod use_declaration;
pub mod expression;
pub mod field;
pub mod for_statement;
pub mod function_call;
pub mod function_declaration;
pub mod global_declaration;
pub mod global_variable_declaration;
pub mod identifier;
pub mod if_statement;
pub mod loop_statement;
pub mod lvalue;
pub mod module_declaration;
pub mod naive;
pub mod parameter;
pub mod path;
pub mod return_statement;
pub mod statement;
pub mod struct_declaration;
pub mod r#type;
pub mod unary_operator;
pub mod variable_declaration;
pub mod while_statement;

pub use super::context::*;
pub use alias_declaration::*;
pub use assignment_statement::*;
pub use ast::*;
pub use binary_operator::*;
pub use block::*;
pub use use_declaration::*;
pub use expression::*;
pub use field::*;
pub use for_statement::*;
pub use function_call::*;
pub use function_declaration::*;
pub use global_declaration::*;
pub use global_variable_declaration::*;
pub use identifier::*;
pub use if_statement::*;
pub use loop_statement::*;
pub use lvalue::*;
pub use module_declaration::*;
pub use naive::*;
pub use parameter::*;
pub use path::*;
pub use r#type::*;
pub use return_statement::*;
pub use statement::*;
pub use struct_declaration::*;
pub use unary_operator::*;
pub use variable_declaration::*;
pub use while_statement::*;

use pest::iterators::{Pair, Pairs};
pub use pest::Parser;

pub use super::GrimeParser;
use super::Rule;
pub use crate::build;

#[macro_export]
macro_rules! build {
	($str:expr, $rule:ident) => {{
		let ctx = Context::new("".as_ref(), $str);
		let ctx1 = ctx.clone();
		let ctx2 = ctx.clone();
		let pairs = GrimeParser::parse(Rule::$rule, ctx.input()).expect("Failed to parse");
		(
			AstNode::build(ctx1, pairs.expect_single()),
			ctx2.as_location(),
		)
	}};
}

pub trait AstNode {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self;
	fn loc(&self) -> &Location;
	fn loc_mut(&mut self) -> &mut Location;
}

pub trait PairExt<'a> {
	fn expect_rule(&self, rule: Rule);
}

pub trait VecExt<T> {
	fn to_array<const N: usize>(self) -> [T; N];
}

pub trait PairsExt<'a> {
	fn expect_next(&mut self) -> Pair<'_, Rule>;
	fn expect_next_rule(&mut self, rule: Rule) -> Pair<'_, Rule> {
		let pair = self.expect_next();
		pair.expect_rule(rule);
		pair
	}
	fn expect_single(self) -> Pair<'a, Rule>;
}

impl<'a> PairExt<'a> for Pair<'a, Rule> {
	fn expect_rule(&self, rule: Rule) {
		assert_eq!(self.as_rule(), rule);
	}
}

impl<'a> PairsExt<'a> for Pairs<'a, Rule> {
	fn expect_next(&mut self) -> Pair<'_, Rule> {
		self.next().expect("Expected another pair")
	}

	fn expect_single(mut self) -> Pair<'a, Rule> {
		let pair = self.next().expect("Expected a pair");
		assert!(self.next().is_none());
		pair
	}
}

impl<T> VecExt<T> for Vec<T> {
	fn to_array<const N: usize>(self) -> [T; N] {
		self.try_into().unwrap_or_else(|v: Self| {
			panic!("Expected a Vec of length {} but it was {}", N, v.len())
		})
	}
}
