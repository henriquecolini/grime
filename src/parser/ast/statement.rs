use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Block(Block),
	VarDeclaration(VariableDeclaration),
	Assignment(AssignStatement),
	Return(ReturnStatement),
	FunctionCall(FunctionCall),
	If(IfStatement),
	While(WhileStatement),
	Loop(LoopStatement),
	For(ForStatement),
}

impl AstNode for Statement {
	fn build(ctx: Context, parent: Pair<'_, Rule>) -> Self {
		parent.expect_rule(Rule::statement);
		let child = parent.into_inner().expect_single();
		match child.as_rule() {
			Rule::block => Statement::Block(Block::build(ctx, child)),
			Rule::var_decl => Statement::VarDeclaration(VariableDeclaration::build(ctx, child)),
			Rule::assign_statement => Statement::Assignment(AssignStatement::build(ctx, child)),
			Rule::return_statement => Statement::Return(ReturnStatement::build(ctx, child)),
			Rule::if_statement => Statement::If(IfStatement::build(ctx, child)),
			Rule::if_else_statement => Statement::If(IfStatement::build(ctx, child)),
			Rule::while_statement => Statement::While(WhileStatement::build(ctx, child)),
			Rule::loop_statement => Statement::Loop(LoopStatement::build(ctx, child)),
			Rule::for_statement => Statement::For(ForStatement::build(ctx, child)),
			Rule::func_statement => {
				let mut children = child.into_inner();
				Statement::FunctionCall(FunctionCall::build(ctx, children.expect_next()))
			}
			_ => unreachable!(),
		}
	}
	fn loc(&self) -> &Location {
		match self {
			Statement::Block(block) => block.loc(),
			Statement::VarDeclaration(var_decl) => var_decl.loc(),
			Statement::Assignment(assign) => assign.loc(),
			Statement::Return(ret) => ret.loc(),
			Statement::FunctionCall(func) => func.loc(),
			Statement::If(if_stmt) => if_stmt.loc(),
			Statement::While(while_stmt) => while_stmt.loc(),
			Statement::Loop(loop_stmt) => loop_stmt.loc(),
			Statement::For(for_stmt) => for_stmt.loc(),
		}
	}
	fn loc_mut(&mut self) -> &mut Location {
		match self {
			Statement::Block(block) => block.loc_mut(),
			Statement::VarDeclaration(var_decl) => var_decl.loc_mut(),
			Statement::Assignment(assign) => assign.loc_mut(),
			Statement::Return(ret) => ret.loc_mut(),
			Statement::FunctionCall(func) => func.loc_mut(),
			Statement::If(if_stmt) => if_stmt.loc_mut(),
			Statement::While(while_stmt) => while_stmt.loc_mut(),
			Statement::Loop(loop_stmt) => loop_stmt.loc_mut(),
			Statement::For(for_stmt) => for_stmt.loc_mut(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_empty() {
		let (res, _): (Statement, _) = build!(r#"{}"#, statement);
		let Statement::Block(block) = res else {
			panic!("Expected Statement::Block, got {:?}", res);
		};
		assert_eq!(block.statements, []);
	}

	#[test]
	fn test_var_declaration() {
		let (res, _): (Statement, _) = build!(r#"let x = 5;"#, statement);
		let Statement::VarDeclaration(var_decl) = res else {
			panic!("Expected Statement::VarDeclaration, got {:?}", res);
		};
		let VariableDeclaration::Initialized {
			name, ty, value, ..
		} = var_decl
		else {
			panic!(
				"Expected VariableDeclaration::Initialized, got {:?}",
				var_decl
			);
		};
		assert_eq!(name.naive(), "x");
		assert_eq!(ty, None);
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_assignment() {
		let (res, _): (Statement, _) = build!(r#"x = 5;"#, statement);
		let Statement::Assignment(assign) = res else {
			panic!("Expected Statement::Assignment, got {:?}", res);
		};
		let AssignStatement { lvalue, value, .. } = assign;
		assert_eq!(lvalue.naive(), lvar(["x"]));
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_return() {
		let (res, _): (Statement, _) = build!(r#"return 5;"#, statement);
		let Statement::Return(ret) = res else {
			panic!("Expected Statement::Return, got {:?}", res);
		};
		let ReturnStatement::Value { value, .. } = ret else {
			panic!("Expected ReturnStatement::Value, got {:?}", ret);
		};
		assert_eq!(value.naive(), integer(5));
	}

	#[test]
	fn test_return_none() {
		let (res, _): (Statement, _) = build!(r#"return;"#, statement);
		let Statement::Return(ret) = res else {
			panic!("Expected Statement::Return, got {:?}", res);
		};
		let ReturnStatement::Unit { .. } = ret else {
			panic!("Expected ReturnStatement::Unit, got {:?}", ret);
		};
	}

	#[test]
	fn test_function_call() {
		let (res, _): (Statement, _) = build!(r#"foo();"#, statement);
		let Statement::FunctionCall(func) = res else {
			panic!("Expected Statement::FunctionCall, got {:?}", res);
		};
		let FunctionCall { name, args, .. } = func;
		assert_eq!(name.naive(), ["foo"]);
		assert_eq!(args, []);
	}

	#[test]
	fn test_if() {
		let (res, _): (Statement, _) = build!(r#"if true {}"#, statement);
		let Statement::If(if_stmt) = res else {
			panic!("Expected Statement::If, got {:?}", res);
		};
		let IfStatement {
			condition,
			then_block,
			else_block,
			..
		} = if_stmt;
		assert_eq!(condition.naive(), boolean(true));
		assert_eq!(then_block.statements, []);
		assert_eq!(else_block, None);
	}

	#[test]
	fn test_if_else() {
		let (res, _): (Statement, _) = build!(r#"if true {} else {}"#, statement);
		let Statement::If(if_stmt) = res else {
			panic!("Expected Statement::If, got {:?}", res);
		};
		let IfStatement {
			condition,
			then_block,
			else_block,
			..
		} = if_stmt;
		let Block { statements, .. } = else_block.unwrap();
		assert_eq!(condition.naive(), boolean(true));
		assert_eq!(then_block.statements, []);
		assert_eq!(statements, []);
	}

	#[test]
	fn test_while() {
		let (res, _): (Statement, _) = build!(r#"while true {}"#, statement);
		let Statement::While(while_stmt) = res else {
			panic!("Expected Statement::While, got {:?}", res);
		};
		let WhileStatement {
			condition, body, ..
		} = while_stmt;
		assert_eq!(condition.naive(), boolean(true));
		assert_eq!(body.statements, []);
	}

	#[test]
	fn test_loop() {
		let (res, _): (Statement, _) = build!(r#"loop {}"#, statement);
		let Statement::Loop(loop_stmt) = res else {
			panic!("Expected Statement::Loop, got {:?}", res);
		};
		let LoopStatement { body, .. } = loop_stmt;
		assert_eq!(body.statements, []);
	}

	#[test]
	fn test_for() {
		let (res, _): (Statement, _) = build!(r#"for x in y {}"#, statement);
		let Statement::For(for_stmt) = res else {
			panic!("Expected Statement::For, got {:?}", res);
		};
		let ForStatement {
			variable,
			iterable,
			body,
			..
		} = for_stmt;
		assert_eq!(variable.naive(), lvar(["x"]));
		assert_eq!(iterable.naive(), var(["y"]));
		assert_eq!(body.statements, []);
	}
}
