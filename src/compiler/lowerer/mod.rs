use crate::compiler::error::FilePosition;
use crate::compiler::parser::ast::{Assignment, ASTBlock, ASTExpression, ASTOperator, ASTStatement, ASTType, AST};
// Lowerer simplifies AST so that it doesn't contain any syntax sugar.

pub fn lower_ast(mut ast: AST) -> AST {
    ast.functions = ast.functions.into_iter().map(|(mut sign, block)| {
        sign.name = transform_function_name(sign.name);
        let block = lower_block(block);
        (sign, block)
    }).collect();

    for structure in &ast.structs {
        for (sign, block) in &structure.methods {
            let mut sign = sign.clone();
            let block = block.clone();

            sign.name = transform_method_name(sign.name);
            let typ = ASTType::Reference(Box::new(ASTType::Struct(structure.name.clone())));
            sign.args.insert(0, ("self".to_string(), typ));
            let block = lower_block(block);

            ast.functions.push((sign, block));
        }
    }

    ast
}

pub fn transform_function_name(name: String) -> String {
    format!("f{name}")
}

pub fn transform_method_name(name: String) -> String {
    format!("m{name}")
}

/* transform a += b into
{
    tmp = &a
    :tmp = :tmp + a
}
 */
fn gen_op_block(pos: FilePosition, op: ASTOperator, assign_to: ASTExpression, value: ASTExpression) -> ASTStatement {
    let var_name = "$tmp".to_string();
    ASTStatement::Block(ASTBlock {
        children: vec![
            ASTStatement::Assignment(Assignment::Assign(
                ASTExpression::Variable(var_name.clone(), pos.clone()),
                ASTExpression::Reference(Box::new(assign_to), pos.clone()),
                pos.clone(),
            )),
            ASTStatement::Assignment(Assignment::Assign(
                ASTExpression::Dereference(Box::new(ASTExpression::Variable(var_name.clone(), pos.clone())), pos.clone()),
                ASTExpression::BinaryOperation(Box::new(ASTExpression::Dereference(Box::new(ASTExpression::Variable(var_name.clone(), pos.clone())), pos.clone())), op, Box::new(value), pos.clone()),
                pos.clone(),
            )),
        ]
    })
}

fn lower_expression(expression: ASTExpression) -> ASTExpression {
    match expression {
        ASTExpression::Integer(_) |
        ASTExpression::Float(_) |
        ASTExpression::String(_) |
        ASTExpression::Boolean(_) |
        ASTExpression::Variable(_, _) => expression,
        ASTExpression::Reference(mut expr, pos) => {
            *expr = lower_expression(*expr);
            ASTExpression::Reference(expr, pos)
        }
        ASTExpression::FunctionCall(name, args) => {
            let args = args.into_iter().map(lower_expression).collect();
            let name = transform_function_name(name);
            ASTExpression::FunctionCall(name, args)
        }
        ASTExpression::StructInitialization(name, args) => {
            let args = args.into_iter().map(lower_expression).collect();
            ASTExpression::StructInitialization(name, args)
        }
        ASTExpression::FieldAccess(mut expr, field, pos) => {
            // auto deref struct so that you can easily access fields of a reference
            *expr = ASTExpression::AutoRef(Box::new(lower_expression(*expr)));
            ASTExpression::FieldAccess(expr, field, pos)
        }
        ASTExpression::MethodCall(expr, _pos, name, args) => {
            let mut args: Vec<ASTExpression> = args.into_iter().map(lower_expression).collect();
            let expr = ASTExpression::AutoRef(Box::new(lower_expression(*expr)));
            args.insert(0, expr);
            let name = transform_method_name(name);

            ASTExpression::FunctionCall(name, args)
        }
        ASTExpression::Dereference(mut expr, pos) => {
            *expr = lower_expression(*expr);
            ASTExpression::Dereference(expr, pos)
        }
        ASTExpression::BinaryOperation(mut expr1, op, mut expr2, pos) => {
            *expr1 = lower_expression(*expr1);
            *expr2 = lower_expression(*expr2);
            ASTExpression::BinaryOperation(expr1, op, expr2, pos)
        }
        ASTExpression::AutoRef(mut expr) => {
            *expr = lower_expression(*expr);
            ASTExpression::AutoRef(expr)
        }
    }
}

fn lower_statement(statement: ASTStatement) -> ASTStatement {
    match statement {
        ASTStatement::Assignment(assignment) => {
            match assignment {
                Assignment::Assign(assign_to, expr, pos) =>
                    ASTStatement::Assignment(Assignment::Assign(lower_expression(assign_to), lower_expression(expr), pos)),
                Assignment::Increase(assign_to, expr, pos) => {
                    let block = gen_op_block(pos, ASTOperator::Plus, assign_to, expr);
                    lower_statement(block)
                }
                Assignment::Decrease(assign_to, expr, pos) => {
                    let block = gen_op_block(pos, ASTOperator::Minus, assign_to, expr);
                    lower_statement(block)
                }
                Assignment::Increment(assign_to, pos) =>
                    lower_statement(ASTStatement::Assignment(Assignment::Increase(assign_to, ASTExpression::Integer(1), pos))),
                Assignment::Decrement(assign_to, pos) =>
                    lower_statement(ASTStatement::Assignment(Assignment::Decrease(assign_to, ASTExpression::Integer(1), pos))),
            }
        }
        ASTStatement::Block(mut block) => {
            block.children = block.children.into_iter().map(|statement| lower_statement(statement)).collect();
            ASTStatement::Block(block)
        }
        ASTStatement::If(mut stat) => {
            stat.block = lower_block(stat.block);
            stat.else_block = stat.else_block.map(lower_block);
            stat.condition = lower_expression(stat.condition);
            ASTStatement::If(stat)
        }
        ASTStatement::While(mut stat) => {
            stat.block = lower_block(stat.block);
            stat.condition = lower_expression(stat.condition);
            ASTStatement::While(stat)
        }
        ASTStatement::Return(mut expr, pos) => {
            expr = expr.map(lower_expression);
            ASTStatement::Return(expr, pos)
        }
        ASTStatement::Print(mut expr) => {
            expr.values = expr.values.into_iter().map(lower_expression).collect();
            ASTStatement::Print(expr)
        }
        ASTStatement::Expression(expr) =>
            ASTStatement::Expression(lower_expression(expr))
    }
}

fn lower_block(mut block: ASTBlock) -> ASTBlock {
    block.children = block.children.into_iter().map(lower_statement).collect();
    block
}