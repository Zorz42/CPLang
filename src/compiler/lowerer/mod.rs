use crate::compiler::error::FilePosition;
use crate::compiler::parser::ast::{
    AST, ASTBlock, ASTExpression, ASTOperator, ASTStatement, ASTType,
};
// Lowerer simplifies AST so that it doesn't contain any syntax sugar.

pub fn lower_ast(mut ast: AST) -> AST {
    ast.functions = ast
        .functions
        .into_iter()
        .map(|(mut sign, block)| {
            sign.name = transform_function_name(sign.name);
            let block = lower_block(block);
            (sign, block)
        })
        .collect();

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
fn gen_op_block(
    pos: FilePosition,
    op: ASTOperator,
    assign_to: ASTExpression,
    value: ASTExpression,
) -> ASTStatement {
    let var_name = "$tmp".to_string();
    ASTStatement::Block {
        block: ASTBlock {
            children: vec![
                ASTStatement::Assignment {
                    assign_to: ASTExpression::Variable(var_name.clone(), pos.clone()),
                    value: ASTExpression::Reference {
                        expr: Box::new(assign_to),
                        pos: pos.clone(),
                    },
                    pos: pos.clone(),
                },
                ASTStatement::Assignment {
                    assign_to: ASTExpression::Dereference {
                        expr: Box::new(ASTExpression::Variable(var_name.clone(), pos.clone())),
                        pos: pos.clone(),
                    },
                    value: ASTExpression::BinaryOperation {
                        expr1: Box::new(ASTExpression::Dereference {
                            expr: Box::new(ASTExpression::Variable(var_name.clone(), pos.clone())),
                            pos: pos.clone(),
                        }),
                        operator: op,
                        expr2: Box::new(value),
                        pos: pos.clone(),
                    },
                    pos: pos.clone(),
                },
            ],
        },
    }
}

fn lower_expression(expression: ASTExpression) -> ASTExpression {
    match expression {
        ASTExpression::Integer(_)
        | ASTExpression::Float(_)
        | ASTExpression::String(_)
        | ASTExpression::Boolean(_)
        | ASTExpression::Variable(_, _) => expression,
        ASTExpression::Reference {
            mut expr,
            pos,
        } => {
            *expr = lower_expression(*expr);
            ASTExpression::Reference {
                expr,
                pos,
            }
        }
        ASTExpression::FunctionCall {
            name,
            arguments,
        } => {
            let arguments = arguments.into_iter().map(lower_expression).collect();
            let name = transform_function_name(name);
            ASTExpression::FunctionCall {
                name,
                arguments,
            }
        }
        ASTExpression::StructInitialization {
            name,
            fields,
        } => {
            let fields = fields.into_iter().map(lower_expression).collect();
            ASTExpression::StructInitialization {
                name,
                fields,
            }
        }
        ASTExpression::FieldAccess {
            mut expr,
            field_name,
            pos,
        } => {
            // auto deref struct so that you can easily access fields of a reference
            *expr = ASTExpression::AutoRef {
                expr: Box::new(lower_expression(*expr)),
            };
            ASTExpression::FieldAccess {
                expr,
                field_name,
                pos,
            }
        }
        ASTExpression::MethodCall {
            expr,
            pos: _,
            method_name,
            arguments,
        } => {
            let mut arguments: Vec<ASTExpression> =
                arguments.into_iter().map(lower_expression).collect();
            let expr = ASTExpression::AutoRef {
                expr: Box::new(lower_expression(*expr)),
            };
            arguments.insert(0, expr);
            let name = transform_method_name(method_name);

            ASTExpression::FunctionCall {
                name,
                arguments,
            }
        }
        ASTExpression::Dereference {
            mut expr,
            pos,
        } => {
            *expr = lower_expression(*expr);
            ASTExpression::Dereference {
                expr,
                pos,
            }
        }
        ASTExpression::BinaryOperation {
            mut expr1,
            operator,
            mut expr2,
            pos,
        } => {
            *expr1 = lower_expression(*expr1);
            *expr2 = lower_expression(*expr2);
            ASTExpression::BinaryOperation {
                expr1,
                operator,
                expr2,
                pos,
            }
        }
        ASTExpression::AutoRef {
            mut expr,
        } => {
            *expr = lower_expression(*expr);
            ASTExpression::AutoRef {
                expr,
            }
        }
    }
}

fn lower_statement(statement: ASTStatement) -> ASTStatement {
    match statement {
        ASTStatement::Assignment {
            assign_to,
            value,
            pos,
        } => ASTStatement::Assignment {
            assign_to: lower_expression(assign_to),
            value: lower_expression(value),
            pos,
        },
        ASTStatement::AssignmentOperator {
            assign_to,
            value,
            operator,
            pos,
        } => {
            let block = gen_op_block(pos, operator, assign_to, value);
            lower_statement(block)
        }
        ASTStatement::AssignmentIncrement {
            assign_to,
            pos,
        } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::Integer(1),
            operator: ASTOperator::Plus,
            pos,
        }),
        ASTStatement::AssignmentDecrement {
            assign_to,
            pos,
        } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::Integer(1),
            operator: ASTOperator::Minus,
            pos,
        }),
        ASTStatement::Block {
            mut block,
        } => {
            block.children =
                block.children.into_iter().map(|statement| lower_statement(statement)).collect();
            ASTStatement::Block {
                block,
            }
        }
        ASTStatement::If {
            mut condition,
            mut block,
            mut else_block,
        } => {
            block = lower_block(block);
            else_block = else_block.map(lower_block);
            condition = lower_expression(condition);
            ASTStatement::If {
                condition,
                block,
                else_block,
            }
        }
        ASTStatement::While {
            mut block,
            mut condition,
        } => {
            block = lower_block(block);
            condition = lower_expression(condition);
            ASTStatement::While {
                block,
                condition,
            }
        }
        ASTStatement::Return {
            mut return_value,
            pos,
        } => {
            return_value = return_value.map(lower_expression);
            ASTStatement::Return {
                return_value,
                pos,
            }
        }
        ASTStatement::Print {
            mut values,
        } => {
            values = values.into_iter().map(lower_expression).collect();
            ASTStatement::Print {
                values,
            }
        }
        ASTStatement::Expression {
            expr,
        } => ASTStatement::Expression {
            expr: lower_expression(expr),
        },
    }
}

fn lower_block(mut block: ASTBlock) -> ASTBlock {
    block.children = block.children.into_iter().map(lower_statement).collect();
    block
}
