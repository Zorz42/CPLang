use crate::compiler::error::FilePosition;
use crate::compiler::parser::ast::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunctionCall, ASTOperator, ASTStatement, ASTStructDeclaration, ASTType, Ast,
};
use std::collections::{HashMap, HashSet};
// Lowerer simplifies AST so that it doesn't contain any syntax sugar.

struct Lowerer {
    used_tuples: HashSet<usize>,
    struct_fields: HashMap<String, Vec<String>>,
    tmp_index: usize,
}

fn gen_tuple_name(tuple_size: usize) -> String {
    format!("{tuple_size}-tuple")
}

pub fn lower_ast(mut ast: Ast) -> Ast {
    let mut lowerer = Lowerer {
        used_tuples: HashSet::new(),
        struct_fields: HashMap::new(),
        tmp_index: 0,
    };

    for structure in &ast.structs {
        let fields = structure.fields.clone().into_iter().map(|(name, _)| name).collect::<Vec<_>>();
        lowerer.struct_fields.insert(structure.name.clone(), fields);
    }

    for structure in &mut ast.structs {
        for (_, typ) in &mut structure.fields {
            *typ = lowerer.lower_type(typ.clone());
        }
    }

    ast.functions = ast
        .functions
        .into_iter()
        .map(|(sign, block)| {
            let block = lowerer.lower_block(block);
            (sign, block)
        })
        .collect();

    for structure in &ast.structs {
        for (sign, block) in &structure.methods {
            let mut sign = sign.clone();
            let block = block.clone();

            sign.name = transform_method_name(sign.name);
            let mut struct_template = structure.template.clone();
            let struct_template_types = struct_template
                .clone()
                .into_iter()
                .map(|(name, pos)| ASTType::Identifier(name, pos, Vec::new()))
                .collect::<Vec<_>>();
            let struct_template_types = lowerer.lower_types(struct_template_types);

            sign.template.append(&mut struct_template);
            let typ = ASTType::Reference(Box::new(ASTType::Identifier(structure.name.clone(), sign.pos, struct_template_types)), sign.pos);
            let typ = lowerer.lower_type(typ);
            sign.args.insert(0, ("self".to_string(), typ, sign.pos));

            let block = lowerer.lower_block(block);

            ast.functions.push((sign, block));
        }
    }

    for tuple_size in lowerer.used_tuples {
        let mut fields = Vec::new();
        let mut template = Vec::new();

        for i in 0..tuple_size {
            let typ_ident = format!("T{i}");
            fields.push((i.to_string(), ASTType::Identifier(typ_ident.clone(), FilePosition::unknown(), Vec::new())));
            template.push((typ_ident, FilePosition::unknown()));
        }

        ast.structs.push(ASTStructDeclaration {
            name: gen_tuple_name(tuple_size),
            fields,
            pre_methods: Vec::new(),
            methods: Vec::new(),
            template,
        });
    }

    ast
}

pub fn transform_method_name(name: String) -> String {
    format!("Method:{name}")
}

impl Lowerer {
    fn new_tmp_name(&mut self) -> String {
        self.tmp_index += 1;
        format!("$tmp{}", self.tmp_index - 1)
    }

    fn lower_types(&mut self, types: Vec<ASTType>) -> Vec<ASTType> {
        let mut res = Vec::new();
        for typ in types {
            res.push(self.lower_type(typ));
        }
        res
    }

    fn lower_type(&mut self, typ: ASTType) -> ASTType {
        match typ {
            ASTType::Any(_) | ASTType::Primitive(_, _) => typ,
            ASTType::Reference(typ, pos) => ASTType::Reference(Box::new(self.lower_type(*typ)), pos),
            ASTType::Identifier(name, pos, typ) => ASTType::Identifier(name, pos, self.lower_types(typ)),
            ASTType::Tuple(types, pos) => {
                let types = self.lower_types(types);
                self.touch_tuple(types.len());
                ASTType::Identifier(gen_tuple_name(types.len()), pos, types)
            }
        }
    }

    /* transform a += b into
    {
        $tmp = &a
        :$tmp = :$tmp + b
    }
     */
    fn gen_op_block(&mut self, pos: FilePosition, operator: ASTOperator, assign_to: ASTExpression, value: ASTExpression) -> ASTStatement {
        let var_name = self.new_tmp_name();
        let var_expr = ASTExpression::new(ASTExpressionKind::Variable(var_name), pos);
        ASTStatement::Block {
            block: ASTBlock {
                children: vec![
                    ASTStatement::Assignment {
                        assign_to: var_expr.clone(),
                        value: ASTExpression::new(ASTExpressionKind::Reference(Box::new(assign_to)), pos),
                        pos,
                    },
                    ASTStatement::Assignment {
                        assign_to: ASTExpression::new(ASTExpressionKind::Dereference(Box::new(var_expr.clone())), pos),
                        value: ASTExpression::new(
                            ASTExpressionKind::BinaryOperation {
                                expression1: Box::new(ASTExpression::new(ASTExpressionKind::Dereference(Box::new(var_expr)), pos)),
                                operator,
                                expression2: Box::new(value),
                            },
                            pos,
                        ),
                        pos,
                    },
                ],
            },
        }
    }

    /* transform MyStruct f1 x f2 y f3 z = x into
    $tmp = x
    x = $tmp.f1
    y = $tmp.f2
    z = $tmp.f3
     */
    fn gen_struct_destructuring(
        &mut self,
        pos: FilePosition,
        value: ASTExpression,
        name: &str,
        fields: &[ASTExpression],
        template_arguments: &[ASTType],
    ) -> ASTStatement {
        let tmp_name = self.new_tmp_name();
        let mut block = Vec::new();

        block.push(ASTStatement::Assignment {
            assign_to: ASTExpression {
                kind: ASTExpressionKind::TypeHint {
                    expression: Box::new(ASTExpression {
                        kind: ASTExpressionKind::Variable(tmp_name.clone()),
                        pos,
                    }),
                    type_hint: ASTType::Identifier(name.to_owned(), pos, template_arguments.to_vec()),
                },
                pos,
            },
            value,
            pos,
        });

        let field_names = self.struct_fields[name].clone();
        for (field_name, field_value) in field_names.into_iter().zip(fields) {
            block.push(ASTStatement::Assignment {
                assign_to: field_value.clone(),
                value: ASTExpression {
                    kind: ASTExpressionKind::FieldAccess {
                        expression: Box::new(ASTExpression {
                            kind: ASTExpressionKind::Variable(tmp_name.clone()),
                            pos,
                        }),
                        field_name,
                    },
                    pos,
                },
                pos,
            });
        }

        ASTStatement::SemiBlock {
            block: ASTBlock { children: block },
        }
    }

    fn gen_for_statement(&mut self, iterator: String, element: ASTExpression, block: ASTBlock, pos: FilePosition) -> ASTStatement {
        // lower for i x { block } into
        // {
        //   $iter = x.iter()
        //   while $iter.has_next() {
        //     i = $iter.next()
        //     block
        //   }
        // }
        let iter_name = self.new_tmp_name();
        self.lower_statement(
            ASTStatement::Block {
                block: ASTBlock {
                    children: vec![
                        ASTStatement::Assignment {
                            assign_to: ASTExpression::new(ASTExpressionKind::Variable(iter_name.clone()), pos),
                            value: ASTExpression::new(ASTExpressionKind::MethodCall {
                                expression: Box::new(element),
                                call: ASTFunctionCall {
                                    name: "iter".to_string(),
                                    arguments: Vec::new(),
                                    template_arguments: Vec::new(),
                                },
                            }, pos),
                            pos,
                        },
                        ASTStatement::While {
                            condition: ASTExpression::new(ASTExpressionKind::MethodCall {
                                expression: Box::new(ASTExpression::new(ASTExpressionKind::Variable(iter_name.clone()), pos)),
                                call: ASTFunctionCall {
                                    name: "has_next".to_string(),
                                    arguments: Vec::new(),
                                    template_arguments: Vec::new(),
                                },
                            }, pos),
                            block: ASTBlock {
                                children: vec![
                                    ASTStatement::Assignment {
                                        assign_to: ASTExpression::new(ASTExpressionKind::Variable(iterator), pos),
                                        value: ASTExpression::new(ASTExpressionKind::MethodCall {
                                            expression: Box::new(ASTExpression::new(ASTExpressionKind::Variable(iter_name), pos)),
                                            call: ASTFunctionCall {
                                                name: "next".to_string(),
                                                arguments: Vec::new(),
                                                template_arguments: Vec::new(),
                                            },
                                        }, pos),
                                        pos,
                                    },
                                    ASTStatement::Block { block },
                                ]
                            }
                        }
                    ]
                }
            }
        )
    }

    fn lower_expressions(&mut self, expressions: Vec<ASTExpression>) -> Vec<ASTExpression> {
        let mut res = Vec::new();
        for expr in expressions {
            res.push(self.lower_expression(expr));
        }
        res
    }

    fn touch_tuple(&mut self, tuple_size: usize) {
        if self.used_tuples.insert(tuple_size) {
            let mut fields = Vec::new();
            for i in 0..tuple_size {
                fields.push(i.to_string());
            }
            self.struct_fields.insert(gen_tuple_name(tuple_size), fields);
        }
    }

    fn lower_expression(&mut self, expression: ASTExpression) -> ASTExpression {
        let pos = expression.pos;
        match expression.kind {
            ASTExpressionKind::Integer(_)
            | ASTExpressionKind::Float(_)
            | ASTExpressionKind::String(_)
            | ASTExpressionKind::Boolean(_)
            | ASTExpressionKind::Variable(_) => expression,

            ASTExpressionKind::TupleInitialization(expressions) => {
                self.touch_tuple(expressions.len());

                self.lower_expression(ASTExpression::new(
                    ASTExpressionKind::StructInitialization {
                        name: gen_tuple_name(expressions.len()),
                        fields: expressions,
                        template_arguments: Vec::new(),
                    },
                    pos,
                ))
            }

            ASTExpressionKind::Reference(mut expression) => {
                *expression = self.lower_expression(*expression);
                ASTExpression::new(ASTExpressionKind::Reference(expression), pos)
            }
            ASTExpressionKind::FunctionCall(mut call) => {
                call.arguments = self.lower_expressions(call.arguments);
                ASTExpression::new(ASTExpressionKind::FunctionCall(call), pos)
            }
            ASTExpressionKind::StructInitialization {
                name,
                fields,
                template_arguments,
            } => ASTExpression::new(
                ASTExpressionKind::StructInitialization {
                    name,
                    fields: self.lower_expressions(fields),
                    template_arguments: self.lower_types(template_arguments),
                },
                pos,
            ),
            ASTExpressionKind::FieldAccess { mut expression, field_name } => {
                // auto deref struct so that you can easily access fields of a reference
                let expr_pos = expression.pos;
                *expression = ASTExpression::new(ASTExpressionKind::AutoRef(Box::new(self.lower_expression(*expression))), expr_pos);
                ASTExpression::new(ASTExpressionKind::FieldAccess { expression, field_name }, pos)
            }
            ASTExpressionKind::TupleAccess { expression, field_index } => self.lower_expression(ASTExpression::new(
                ASTExpressionKind::FieldAccess {
                    expression,
                    field_name: field_index.to_string(),
                },
                pos,
            )),
            ASTExpressionKind::MethodCall { expression, mut call } => {
                call.arguments = self.lower_expressions(call.arguments);
                let expression = ASTExpression::new(ASTExpressionKind::AutoRef(Box::new(self.lower_expression(*expression))), pos);
                call.arguments.insert(0, expression);
                call.name = transform_method_name(call.name);

                ASTExpression::new(ASTExpressionKind::FunctionCall(call), pos)
            }
            ASTExpressionKind::Dereference(mut expression) => {
                *expression = self.lower_expression(*expression);
                ASTExpression::new(ASTExpressionKind::Dereference(expression), pos)
            }
            ASTExpressionKind::BinaryOperation {
                expression1: _,
                operator: ASTOperator::Comma,
                expression2: _,
            } => {
                fn flatten_commas(expr: ASTExpression) -> Vec<ASTExpression> {
                    match expr.kind {
                        ASTExpressionKind::BinaryOperation {
                            expression1,
                            operator: ASTOperator::Comma,
                            expression2,
                        } => {
                            let vec1 = flatten_commas(*expression1);
                            let vec2 = flatten_commas(*expression2);
                            [vec1, vec2].concat()
                        }
                        _ => vec![expr],
                    }
                }

                let vec = flatten_commas(expression);

                self.lower_expression(ASTExpression::new(ASTExpressionKind::TupleInitialization(vec), pos))
            }
            ASTExpressionKind::BinaryOperation {
                expression1,
                operator: ASTOperator::DotDot,
                expression2,
            } => {
                // turn a..b into Range from a to b
                self.lower_expression(
                    ASTExpression::new(ASTExpressionKind::StructInitialization {
                        name: "Range".to_string(),
                        fields: vec![*expression1, *expression2],
                        template_arguments: Vec::new(),
                    }, pos)
                )
            }
            ASTExpressionKind::BinaryOperation {
                mut expression1,
                operator,
                mut expression2,
            } => {
                *expression1 = self.lower_expression(*expression1);
                *expression2 = self.lower_expression(*expression2);

                let name = "operator".to_string()
                    + match operator {
                    ASTOperator::Plus => "+",
                    ASTOperator::Mul => "*",
                    ASTOperator::Div => "/",
                    ASTOperator::Equals => "==",
                    ASTOperator::NotEquals => "!=",
                    ASTOperator::Greater => ">",
                    ASTOperator::Lesser => "<",
                    ASTOperator::GreaterEq => ">=",
                    ASTOperator::LesserEq => "<=",
                    ASTOperator::Minus => "-",
                    ASTOperator::Comma
                    | ASTOperator::DotDot => unreachable!(),
                };

                ASTExpression::new(
                    ASTExpressionKind::FunctionCall(ASTFunctionCall {
                        name,
                        arguments: vec![*expression1, *expression2],
                        template_arguments: Vec::new(),
                    }),
                    pos,
                )
            }
            ASTExpressionKind::AutoRef(mut expression) => {
                *expression = self.lower_expression(*expression);
                ASTExpression::new(ASTExpressionKind::AutoRef(expression), pos)
            }
            ASTExpressionKind::TypeHint { mut expression, type_hint } => {
                let type_hint = self.lower_type(type_hint);
                *expression = self.lower_expression(*expression);
                ASTExpression::new(ASTExpressionKind::TypeHint { expression, type_hint }, pos)
            }

            ASTExpressionKind::Index { expression, arguments } => self.lower_expression(ASTExpression {
                kind: ASTExpressionKind::AutoRef(Box::new(ASTExpression {
                    kind: ASTExpressionKind::MethodCall {
                        expression,
                        call: ASTFunctionCall {
                            name: "operator[]".to_owned(),
                            arguments,
                            template_arguments: Vec::new(),
                        },
                    },
                    pos,
                })),
                pos,
            }),
        }
    }

    // lower all statements and also flatten them
    fn lower_statements(&mut self, statements: Vec<ASTStatement>) -> Vec<ASTStatement> {
        let mut res = Vec::new();
        for statement in statements {
            let statement = self.lower_statement(statement);
            match statement {
                ASTStatement::SemiBlock { block } => {
                    for statement in block.children {
                        res.push(statement);
                    }
                }
                _ => res.push(statement),
            }
        }
        res
    }

    fn lower_statement(&mut self, statement: ASTStatement) -> ASTStatement {
        match statement {
            ASTStatement::Assignment { assign_to, value, pos } => {
                let assignment = ASTStatement::Assignment {
                    assign_to: self.lower_expression(assign_to),
                    value: self.lower_expression(value),
                    pos,
                };
                // destructure struct initialization
                match &assignment {
                    ASTStatement::Assignment { assign_to, value, pos } => match &assign_to.kind {
                        ASTExpressionKind::StructInitialization {
                            name,
                            fields,
                            template_arguments,
                        } => {
                            let statement = self.gen_struct_destructuring(*pos, value.clone(), name, fields, template_arguments);
                            self.lower_statement(statement)
                        }
                        _ => assignment,
                    },
                    _ => assignment,
                }
            }
            ASTStatement::AssignmentOperator { assign_to, value, operator } => {
                let block = self.gen_op_block(value.pos, operator, assign_to, value);
                self.lower_statement(block)
            }
            ASTStatement::AssignmentIncrement { assign_to, pos } => self.lower_statement(ASTStatement::AssignmentOperator {
                assign_to,
                value: ASTExpression::new(ASTExpressionKind::Integer(1), pos),
                operator: ASTOperator::Plus,
            }),
            ASTStatement::AssignmentDecrement { assign_to, pos } => self.lower_statement(ASTStatement::AssignmentOperator {
                assign_to,
                value: ASTExpression::new(ASTExpressionKind::Integer(1), pos),
                operator: ASTOperator::Minus,
            }),
            ASTStatement::Block { block } => ASTStatement::Block {
                block: ASTBlock {
                    children: self.lower_statements(block.children),
                },
            },
            ASTStatement::SemiBlock { block } => ASTStatement::SemiBlock {
                block: ASTBlock {
                    children: self.lower_statements(block.children),
                },
            },
            ASTStatement::If { condition, block, else_block } => ASTStatement::If {
                block: self.lower_block(block),
                else_block: else_block.map(|x| self.lower_block(x)),
                condition: self.lower_expression(condition),
            },
            ASTStatement::While { block, condition } => ASTStatement::While {
                block: self.lower_block(block),
                condition: self.lower_expression(condition),
            },
            ASTStatement::Return { return_value, pos } => ASTStatement::Return {
                return_value: return_value.map(|x| self.lower_expression(x)),
                pos,
            },
            ASTStatement::Print { values } => ASTStatement::Print {
                values: self.lower_expressions(values),
            },
            ASTStatement::Expression { expression } => ASTStatement::Expression {
                expression: self.lower_expression(expression),
            },
            ASTStatement::For { iterator, element, block, pos } => {
                self.gen_for_statement(iterator, element, block, pos)
            }
        }
    }

    fn lower_block(&mut self, block: ASTBlock) -> ASTBlock {
        ASTBlock {
            children: self.lower_statements(block.children),
        }
    }
}
