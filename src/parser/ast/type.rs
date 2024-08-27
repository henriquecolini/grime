use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	Simple {
		loc: Location,
		name: Path,
	},
	Array {
		loc: Location,
		ty: Box<Type>,
		size: Expression,
	},
	Slice {
		loc: Location,
		ty: Box<Type>,
	},
	Ref {
		loc: Location,
		ty: Box<Type>,
	},
	RefConst {
		loc: Location,
		ty: Box<Type>,
	},
}

impl AstNode for Type {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		let loc = ctx.paired(&parent);
		parent.expect_rule(Rule::any_type);
		let child = parent.into_inner().expect_single();
		match child.as_rule() {
			Rule::simple_type => Type::Simple {
				loc,
				name: Path::build(ctx, child.into_inner().expect_single()),
			},
			Rule::array_type => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::lsquare);
				let ty = Box::new(Type::build(ctx.clone(), children.expect_next()));
				children.expect_next_rule(Rule::semicolon);
				let size = Expression::build(ctx.clone(), children.expect_next());
				children.expect_next_rule(Rule::rsquare);
				Type::Array { loc, ty, size }
			}
			Rule::slice_type => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::lsquare);
				let ty = Box::new(Type::build(ctx.clone(), children.expect_next()));
				children.expect_next_rule(Rule::rsquare);
				Type::Slice { loc, ty }
			}
			Rule::ref_type => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::r#ref);
				let ty = Box::new(Type::build(ctx.clone(), children.expect_next()));
				Type::Ref { loc, ty }
			}
			Rule::ref_const_type => {
				let mut children = child.into_inner();
				children.expect_next_rule(Rule::ref_const);
				let ty = Box::new(Type::build(ctx.clone(), children.expect_next()));
				Type::RefConst { loc, ty }
			}
			_ => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			Type::Simple { loc, .. }
			| Type::Array { loc, .. }
			| Type::Slice { loc, .. }
			| Type::Ref { loc, .. }
			| Type::RefConst { loc, .. } => loc,
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			Type::Simple { loc, .. }
			| Type::Array { loc, .. }
			| Type::Slice { loc, .. }
			| Type::Ref { loc, .. }
			| Type::RefConst { loc, .. } => loc,
		}
	}
}

impl Naive for Type {
	type Output = NaiveType;
	fn naive(self) -> Self::Output {
		NaiveType::from(self)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_simple() {
		let (res, _): (Type, _) = build!(r#"Foo"#, any_type);
		assert_eq!(res.naive(), tsimple(["Foo"]));
	}

	#[test]
	fn test_array() {
		let (res, _): (Type, _) = build!(r#"[Foo; 42]"#, any_type);
		assert_eq!(res.naive(), tarray(tsimple(["Foo"]), integer(42)));
	}

	#[test]
	fn test_slice() {
		let (res, _): (Type, _) = build!(r#"[Foo]"#, any_type);
		assert_eq!(res.naive(), tslice(tsimple(["Foo"])));
	}

	#[test]
	fn test_ref() {
		let (res, _): (Type, _) = build!(r#"&Foo"#, any_type);
		assert_eq!(res.naive(), tref(tsimple(["Foo"])));
	}

	#[test]
	fn test_ref_const() {
		let (res, _): (Type, _) = build!(r#"&const Foo"#, any_type);
		assert_eq!(res.naive(), tref_const(tsimple(["Foo"])));
	}

	#[test]
	fn test_ref_slice() {
		let (res, _): (Type, _) = build!(r#"&[Foo]"#, any_type);
		assert_eq!(res.naive(), tref(tslice(tsimple(["Foo"]))));
	}

	#[test]
	fn test_ref_slice_of_const_ref() {
		let (res, _): (Type, _) = build!(r#"&const [&Foo]"#, any_type);
		assert_eq!(res.naive(), tref_const(tslice(tref(tsimple(["Foo"])))));
	}

	#[test]
	fn test_slice_of_slices() {
		let (res, _): (Type, _) = build!(r#"[[Foo]]"#, any_type);
		assert_eq!(res.naive(), tslice(tslice(tsimple(["Foo"]))));
	}

	#[test]
	fn test_array_of_refs() {
		let (res, _): (Type, _) = build!(r#"[&Foo; 42]"#, any_type);
		assert_eq!(res.naive(), tarray(tref(tsimple(["Foo"])), integer(42)));
	}
}
