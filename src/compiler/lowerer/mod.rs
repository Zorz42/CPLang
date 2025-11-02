use crate::compiler::parser::{Statement, AST};
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::{Expression, Operator};
use crate::compiler::parser::assignment::{Assignment, AssignmentType};

// Lowerer simply transforms AST into a more simple version that doesn't contain any syntax sugar.

pub fn lower_ast(mut ast: AST) -> AST {
    ast.functions = ast.functions.into_iter().map(|(sign, block)| {
        (sign, lower_block(block))
    }).collect::<Vec<_>>();

    ast
}

fn lower_statement(statement: Statement) -> Statement {
    match statement {
        Statement::Assignment(assignment) => {
            match assignment.typ {
                AssignmentType::Assign => Statement::Assignment(assignment),
                AssignmentType::Increase | AssignmentType::Decrease => {
                    let pos = assignment.pos;
                    let var_name = "tmp".to_string();
                    let op = match assignment.typ {
                        AssignmentType::Increase => Operator::Plus,
                        AssignmentType::Decrease => Operator::Minus,
                        _ => unreachable!(),
                    };
                    let block = Statement::Block(Block {
                        children: vec![
                            Statement::Assignment(Assignment {
                                assign_to: Expression::Variable(var_name.clone(), pos.clone()),
                                value: Expression::Reference(Box::new(assignment.assign_to), pos.clone()),
                                typ: AssignmentType::Assign,
                                pos: pos.clone(),
                            }),
                            Statement::Assignment(Assignment {
                                assign_to: Expression::Dereference(Box::new(Expression::Variable(var_name.clone(), pos.clone())), pos.clone()),
                                value: Expression::BinaryOperation(Box::new(Expression::Variable(var_name.clone(), pos.clone())), op, Box::new(assignment.value), pos.clone()),
                                typ: AssignmentType::Assign,
                                pos: pos.clone(),
                            }),
                        ]
                    });
                    lower_statement(block)
                }
                AssignmentType::Increment => lower_statement(Statement::Assignment(Assignment {
                    assign_to: assignment.assign_to,
                    value: Expression::Integer(1),
                    typ: AssignmentType::Increase,
                    pos: assignment.pos,
                })),
                AssignmentType::Decrement => lower_statement(Statement::Assignment(Assignment {
                    assign_to: assignment.assign_to,
                    value: Expression::Integer(1),
                    typ: AssignmentType::Decrease,
                    pos: assignment.pos,
                })),
            }
        }
        Statement::Block(mut block) => {
            block.children = block.children.into_iter().map(|statement| lower_statement(statement)).collect::<Vec<_>>();
            Statement::Block(block)
        }
        _ => statement
    }
}

fn lower_block(mut block: Block) -> Block {
    block.children = block.children.into_iter().map(lower_statement).collect::<Vec<_>>();
    block
}