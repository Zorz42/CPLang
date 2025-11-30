use crate::compiler::error::FilePosition;
use crate::compiler::parser::ast::{ASTBlock, ASTExpression, ASTOperator, ASTStatement, ASTType, Ast};
// Lowerer simplifies AST so that it doesn't contain any syntax sugar.

pub fn lower_ast(mut ast: Ast) -> Ast {
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
            let typ = ASTType::Reference(Box::new(ASTType::Identifier(structure.name.clone(), sign.pos.clone())), sign.pos.clone());
            sign.args.insert(0, ("self".to_string(), typ, sign.pos.clone()));
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
fn gen_op_block(pos: FilePosition, operator: ASTOperator, assign_to: ASTExpression, value: ASTExpression) -> ASTStatement {
    let var_name = "$tmp".to_string();
    ASTStatement::Block {
        block: ASTBlock {
            children: vec![
                ASTStatement::Assignment {
                    assign_to: ASTExpression::Variable(var_name.clone(), pos.clone()),
                    value: ASTExpression::Reference {
                        expression: Box::new(assign_to),
                        pos: pos.clone(),
                    },
                    pos: pos.clone(),
                },
                ASTStatement::Assignment {
                    assign_to: ASTExpression::Dereference {
                        expression: Box::new(ASTExpression::Variable(var_name.clone(), pos.clone())),
                        pos: pos.clone(),
                    },
                    value: ASTExpression::BinaryOperation {
                        expression1: Box::new(ASTExpression::Dereference {
                            expression: Box::new(ASTExpression::Variable(var_name, pos.clone())),
                            pos: pos.clone(),
                        }),
                        operator,
                        expression2: Box::new(value),
                        pos: pos.clone(),
                    },
                    pos,
                },
            ],
        },
    }
}

fn lower_expression(expression: ASTExpression) -> ASTExpression {
    match expression {
        ASTExpression::Integer(_, _)
        | ASTExpression::Float(_, _)
        | ASTExpression::String(_, _)
        | ASTExpression::Boolean(_, _)
        | ASTExpression::Variable(_, _) => expression,
        ASTExpression::Reference { mut expression, pos } => {
            *expression = lower_expression(*expression);
            ASTExpression::Reference { expression, pos }
        }
        ASTExpression::FunctionCall { name, arguments, pos } => {
            let arguments = arguments.into_iter().map(lower_expression).collect();
            let name = transform_function_name(name);
            ASTExpression::FunctionCall { name, arguments, pos }
        }
        ASTExpression::StructInitialization { name, fields, pos } => {
            let fields = fields.into_iter().map(lower_expression).collect();
            ASTExpression::StructInitialization { name, fields, pos }
        }
        ASTExpression::FieldAccess {
            mut expression,
            field_name,
            pos,
        } => {
            // auto deref struct so that you can easily access fields of a reference
            *expression = ASTExpression::AutoRef {
                expression: Box::new(lower_expression(*expression)),
            };
            ASTExpression::FieldAccess { expression, field_name, pos }
        }
        ASTExpression::MethodCall {
            expression,
            pos,
            method_name,
            arguments,
        } => {
            let mut arguments: Vec<ASTExpression> = arguments.into_iter().map(lower_expression).collect();
            let expression = ASTExpression::AutoRef {
                expression: Box::new(lower_expression(*expression)),
            };
            arguments.insert(0, expression);
            let name = transform_method_name(method_name);

            ASTExpression::FunctionCall { name, arguments, pos }
        }
        ASTExpression::Dereference { mut expression, pos } => {
            *expression = lower_expression(*expression);
            ASTExpression::Dereference { expression, pos }
        }
        ASTExpression::BinaryOperation {
            mut expression1,
            operator,
            mut expression2,
            pos,
        } => {
            *expression1 = lower_expression(*expression1);
            *expression2 = lower_expression(*expression2);
            ASTExpression::BinaryOperation {
                expression1,
                operator,
                expression2,
                pos,
            }
        }
        ASTExpression::AutoRef { mut expression } => {
            *expression = lower_expression(*expression);
            ASTExpression::AutoRef { expression }
        }
    }
}

fn lower_statement(statement: ASTStatement) -> ASTStatement {
    match statement {
        ASTStatement::Assignment { assign_to, value, pos } => ASTStatement::Assignment {
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
        ASTStatement::AssignmentIncrement { assign_to, pos } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::Integer(1, pos.clone()),
            operator: ASTOperator::Plus,
            pos,
        }),
        ASTStatement::AssignmentDecrement { assign_to, pos } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::Integer(1, pos.clone()),
            operator: ASTOperator::Minus,
            pos,
        }),
        ASTStatement::Block { mut block } => {
            block.children = block.children.into_iter().map(lower_statement).collect();
            ASTStatement::Block { block }
        }
        ASTStatement::If {
            mut condition,
            mut block,
            mut else_block,
        } => {
            block = lower_block(block);
            else_block = else_block.map(lower_block);
            condition = lower_expression(condition);
            ASTStatement::If { condition, block, else_block }
        }
        ASTStatement::While { mut block, mut condition } => {
            block = lower_block(block);
            condition = lower_expression(condition);
            ASTStatement::While { block, condition }
        }
        ASTStatement::Return { mut return_value, pos } => {
            return_value = return_value.map(lower_expression);
            ASTStatement::Return { return_value, pos }
        }
        ASTStatement::Print { mut values } => {
            values = values.into_iter().map(lower_expression).collect();
            ASTStatement::Print { values }
        }
        ASTStatement::Expression { expression } => ASTStatement::Expression {
            expression: lower_expression(expression),
        },
    }
}

fn lower_block(mut block: ASTBlock) -> ASTBlock {
    block.children = block.children.into_iter().map(lower_statement).collect();
    block
}
