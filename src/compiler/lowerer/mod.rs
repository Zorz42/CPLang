use crate::compiler::error::FilePosition;
use crate::compiler::parser::{Statement, AST};
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::{Expression, Operator};
use crate::compiler::parser::assignment::Assignment;

// Lowerer simply transforms AST into a more simple version that doesn't contain any syntax sugar.

pub fn lower_ast(mut ast: AST) -> AST {
    ast.functions = ast.functions.into_iter().map(|(sign, block)| {
        (sign, lower_block(block))
    }).collect();

    ast
}

/* transform a += b into
{
    tmp = &a
    :tmp = :tmp + a
}
 */
fn gen_op_block(pos: FilePosition, op: Operator, assign_to: Expression, value: Expression) -> Statement {
    let var_name = "$tmp".to_string();
    Statement::Block(Block {
        children: vec![
            Statement::Assignment(Assignment::Assign(
                Expression::Variable(var_name.clone(), pos.clone()),
                Expression::Reference(Box::new(assign_to), pos.clone()),
                pos.clone(),
            )),
            Statement::Assignment(Assignment::Assign(
                Expression::Dereference(Box::new(Expression::Variable(var_name.clone(), pos.clone())), pos.clone()),
                Expression::BinaryOperation(Box::new(Expression::Dereference(Box::new(Expression::Variable(var_name.clone(), pos.clone())), pos.clone())), op, Box::new(value), pos.clone()),
                pos.clone(),
            )),
        ]
    })
}

fn lower_expression(expression: Expression) -> Expression {
    match expression {
        Expression::Integer(_) |
        Expression::Float(_) |
        Expression::String(_) |
        Expression::Boolean(_) |
        Expression::Variable(_, _) => expression,
        Expression::Reference(mut expr, pos) => {
            *expr = lower_expression(*expr);
            Expression::Reference(expr, pos)
        }
        Expression::FunctionCall(name, args) => {
            let args = args.into_iter().map(lower_expression).collect();
            Expression::FunctionCall(name, args)
        }
        Expression::StructInitialization(name, args) => {
            let args = args.into_iter().map(lower_expression).collect();
            Expression::StructInitialization(name, args)
        }
        Expression::FieldAccess(mut expr, field, pos) => {
            // auto deref struct so that you can easily access fields of a reference
            *expr = Expression::AutoRef(Box::new(lower_expression(*expr)));
            Expression::FieldAccess(expr, field, pos)
        }
        Expression::MethodCall(_, _, _, _) =>
            todo!(),
        Expression::Dereference(mut expr, pos) => {
            *expr = lower_expression(*expr);
            Expression::Dereference(expr, pos)
        }
        Expression::BinaryOperation(mut expr1, op, mut expr2, pos) => {
            *expr1 = lower_expression(*expr1);
            *expr2 = lower_expression(*expr2);
            Expression::BinaryOperation(expr1, op, expr2, pos)
        }
        Expression::AutoRef(mut expr) => {
            *expr = lower_expression(*expr);
            Expression::AutoRef(expr)
        }
    }
}

fn lower_statement(statement: Statement) -> Statement {
    match statement {
        Statement::Assignment(assignment) => {
            match assignment {
                Assignment::Assign(assign_to, expr, pos) =>
                    Statement::Assignment(Assignment::Assign(lower_expression(assign_to), lower_expression(expr), pos)),
                Assignment::Increase(assign_to, expr, pos) => {
                    let block = gen_op_block(pos, Operator::Plus, assign_to, expr);
                    lower_statement(block)
                }
                Assignment::Decrease(assign_to, expr, pos) => {
                    let block = gen_op_block(pos, Operator::Minus, assign_to, expr);
                    lower_statement(block)
                }
                Assignment::Increment(assign_to, pos) =>
                    lower_statement(Statement::Assignment(Assignment::Increase(assign_to, Expression::Integer(1), pos))),
                Assignment::Decrement(assign_to, pos) =>
                    lower_statement(Statement::Assignment(Assignment::Decrease(assign_to, Expression::Integer(1), pos))),
            }
        }
        Statement::Block(mut block) => {
            block.children = block.children.into_iter().map(|statement| lower_statement(statement)).collect();
            Statement::Block(block)
        }
        Statement::If(mut stat) => {
            stat.block = lower_block(stat.block);
            stat.else_block = stat.else_block.map(lower_block);
            stat.condition = lower_expression(stat.condition);
            Statement::If(stat)
        }
        Statement::While(mut stat) => {
            stat.block = lower_block(stat.block);
            stat.condition = lower_expression(stat.condition);
            Statement::While(stat)
        }
        Statement::Return(mut expr, pos) => {
            expr = expr.map(lower_expression);
            Statement::Return(expr, pos)
        }
        Statement::Print(mut expr) => {
            expr.values = expr.values.into_iter().map(lower_expression).collect();
            Statement::Print(expr)
        }
        Statement::Expression(expr) =>
            Statement::Expression(lower_expression(expr))
    }
}

fn lower_block(mut block: Block) -> Block {
    block.children = block.children.into_iter().map(lower_statement).collect();
    block
}