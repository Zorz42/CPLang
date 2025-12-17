use crate::compiler::error::FilePosition;
use crate::compiler::parser::ast::{ASTBlock, ASTExpression, ASTExpressionKind, ASTOperator, ASTStatement, ASTType, Ast};
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
            let mut struct_template = structure.template.clone();
            let struct_template_types = struct_template.clone().into_iter().map(|(name, pos)| ASTType::Identifier(name, pos, Vec::new())).collect::<Vec<_>>();
            sign.template.append(&mut struct_template);
            let typ = ASTType::Reference(Box::new(ASTType::Identifier(structure.name.clone(), sign.pos, struct_template_types)), sign.pos);
            sign.args.insert(0, ("self".to_string(), typ, sign.pos));
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
    let var_expr = ASTExpression::no_hint(
        ASTExpressionKind::Variable(var_name),
        pos,
    );
    ASTStatement::Block {
        block: ASTBlock {
            children: vec![
                ASTStatement::Assignment {
                    assign_to: var_expr.clone(),
                    value: ASTExpression::no_hint(ASTExpressionKind::Reference(Box::new(assign_to)), pos),
                    pos,
                },
                ASTStatement::Assignment {
                    assign_to: ASTExpression::no_hint(ASTExpressionKind::Dereference(Box::new(var_expr.clone())), pos),
                    value: ASTExpression::no_hint(ASTExpressionKind::BinaryOperation {
                        expression1: Box::new(ASTExpression::no_hint(ASTExpressionKind::Dereference(Box::new(var_expr)), pos)),
                        operator,
                        expression2: Box::new(value),
                    }, pos),
                    pos,
                },
            ],
        },
    }
}

fn lower_expression(expression: ASTExpression) -> ASTExpression {
    let pos = expression.pos;
    match expression.kind {
        ASTExpressionKind::Integer(_)
        | ASTExpressionKind::Float(_)
        | ASTExpressionKind::String(_)
        | ASTExpressionKind::Boolean(_)
        | ASTExpressionKind::Variable(_) =>
            ASTExpression {
                kind: expression.kind,
                pos,
                type_hint: expression.type_hint,
            },
        ASTExpressionKind::Reference(mut expression) => {
            *expression = lower_expression(*expression);
            ASTExpression::no_hint(ASTExpressionKind::Reference(expression), pos)
        }
        ASTExpressionKind::FunctionCall(mut call) => {
            call.arguments = call.arguments.into_iter().map(lower_expression).collect();
            call.name = transform_function_name(call.name);
            ASTExpression::no_hint(ASTExpressionKind::FunctionCall(call), pos)
        }
        ASTExpressionKind::StructInitialization { name, fields, template_arguments } => {
            let fields = fields.into_iter().map(lower_expression).collect();
            ASTExpression::no_hint(ASTExpressionKind::StructInitialization { name, fields, template_arguments }, pos)
        }
        ASTExpressionKind::FieldAccess {
            mut expression,
            field_name,
        } => {
            // auto deref struct so that you can easily access fields of a reference
            let expr_pos = expression.pos;
            *expression = ASTExpression::no_hint(ASTExpressionKind::AutoRef(Box::new(lower_expression(*expression))), expr_pos);
            ASTExpression::no_hint(ASTExpressionKind::FieldAccess { expression, field_name }, pos)
        }
        ASTExpressionKind::MethodCall {
            expression,
            mut call,
        } => {
            call.arguments = call.arguments.into_iter().map(lower_expression).collect();
            let expression = ASTExpression::no_hint(ASTExpressionKind::AutoRef(Box::new(lower_expression(*expression))), pos);
            call.arguments.insert(0, expression);
            call.name = transform_method_name(call.name);

            ASTExpression::no_hint(ASTExpressionKind::FunctionCall(call), pos)
        }
        ASTExpressionKind::Dereference(mut expression) => {
            *expression = lower_expression(*expression);
            ASTExpression::no_hint(ASTExpressionKind::Dereference(expression), pos)
        }
        ASTExpressionKind::BinaryOperation {
            mut expression1,
            operator,
            mut expression2,
        } => {
            *expression1 = lower_expression(*expression1);
            *expression2 = lower_expression(*expression2);
            ASTExpression::no_hint(ASTExpressionKind::BinaryOperation {
                expression1,
                operator,
                expression2,
            }, pos)
        }
        ASTExpressionKind::AutoRef(mut expression) => {
            *expression = lower_expression(*expression);
            ASTExpression::no_hint(ASTExpressionKind::AutoRef(expression), pos)
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
        } => {
            let block = gen_op_block(value.pos, operator, assign_to, value);
            lower_statement(block)
        }
        ASTStatement::AssignmentIncrement { assign_to, pos } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::no_hint(ASTExpressionKind::Integer(1), pos),
            operator: ASTOperator::Plus,
        }),
        ASTStatement::AssignmentDecrement { assign_to, pos } => lower_statement(ASTStatement::AssignmentOperator {
            assign_to,
            value: ASTExpression::no_hint(ASTExpressionKind::Integer(1), pos),
            operator: ASTOperator::Minus,
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
