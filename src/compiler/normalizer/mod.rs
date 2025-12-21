use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::lowerer::transform_function_name;
use crate::compiler::normalizer::ir::{
    IRBlock, IRConstant, IRExpression, IRFieldLabel, IRInstance, IRInstanceLabel, IRPrimitiveType, IRStatement, IRStruct, IRStructLabel, IRType,
    IRTypeLabel, IRVariableLabel, IR,
};
use crate::compiler::parser::ast::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunctionSignature, ASTPrimitiveType, ASTStatement, ASTStructDeclaration, ASTType, Ast,
};
use crate::compiler::type_resolver::TypeResolver;
use std::collections::{HashMap, HashSet};
use std::mem::swap;

pub mod builtin_functions;
pub mod ir;
mod ir_debug;
mod function_cmp;

#[derive(PartialEq, Eq)]
enum ValuePhysicality {
    Temporary,
    Physical,
}

pub fn normalize_ast(ast: Ast) -> CompilerResult<IR> {
    let normalizer = Normalizer {
        ir: IR {
            structs: Vec::new(),
            instances: Vec::new(),
            types: HashMap::new(),
            variable_types: Vec::new(),
            main_function: 0,
            autorefs: Vec::new(),
        },
        curr_var_label: 0,
        curr_func_label: 0,
        curr_field_label: 0,
        type_resolver: TypeResolver::new(),
        variables_name_map: HashMap::new(),
        functions_name_map: HashMap::new(),
        functions_specific_ordering: HashMap::new(),
        fields_name_map: HashMap::new(),
        curr_func_vars: Vec::new(),
        curr_func_ret_type: 0,
        has_ret_statement: false,
        depth: 0,
        structs_name_map: HashMap::new(),
        instance_cache: HashMap::new(),
        structs_type_hints: Vec::new(),
        structs_templates: Vec::new(),
        template_types: HashMap::new(),
        relevant_types: Vec::new(),
    };

    normalizer.normalize_ast(ast)
}

struct Normalizer {
    ir: IR,
    type_resolver: TypeResolver,
    variables_name_map: HashMap<String, IRVariableLabel>,
    curr_var_label: IRVariableLabel,
    // key is (function name, number of arguments)
    functions_name_map: HashMap<(String, usize), Vec<(ASTFunctionSignature, ASTBlock)>>,
    // lists all connections in the function ordering graph
    // so if tuple (a, b) exists, it means function a is more specific than b
    functions_specific_ordering: HashMap<(String, usize), Vec<(usize, usize)>>,
    curr_func_label: IRInstanceLabel,
    fields_name_map: HashMap<String, IRFieldLabel>,
    curr_field_label: IRFieldLabel,
    curr_func_vars: Vec<IRVariableLabel>,
    curr_func_ret_type: IRTypeLabel,
    has_ret_statement: bool,
    depth: i32,
    structs_name_map: HashMap<String, IRStructLabel>,
    structs_type_hints: Vec<Vec<ASTType>>,
    structs_templates: Vec<Vec<(String, FilePosition)>>,
    instance_cache: HashMap<String, Vec<(Vec<IRTypeLabel>, IRInstanceLabel)>>,
    template_types: HashMap<String, IRTypeLabel>,
    relevant_types: Vec<IRTypeLabel>,
}

impl Normalizer {
    const fn new_var_label(&mut self) -> IRVariableLabel {
        self.curr_var_label += 1;
        self.curr_var_label - 1
    }

    fn new_var(&mut self, name: &str) -> IRVariableLabel {
        let label = self.new_var_label();
        self.variables_name_map.insert(name.to_string(), label);
        label
    }

    const fn new_field_label(&mut self) -> IRFieldLabel {
        self.curr_field_label += 1;
        self.curr_field_label - 1
    }

    fn new_field(&mut self, name: &str) -> IRFieldLabel {
        let label = self.new_field_label();
        self.fields_name_map.insert(name.to_string(), label);
        label
    }

    fn normalize_ast(mut self, ast: Ast) -> CompilerResult<IR> {
        // add all functions into the map
        for (sig, block) in ast.functions {
            let val = self.functions_name_map.entry((sig.name.clone(), sig.args.len())).or_default();
            val.push((sig, block));
        }

        // normalize all structs
        for structure in ast.structs {
            let name = structure.name.clone();
            let (ir_struct, type_hints, template) = self.normalize_struct(structure);
            let label = self.ir.structs.len() as IRStructLabel;
            self.structs_type_hints.push(type_hints);
            self.structs_templates.push(template);
            self.structs_name_map.insert(name, label);
            self.ir.structs.push(ir_struct);
        }

        // check that there is one main function with zero arguments and find it
        let main_name = transform_function_name("main".to_string());
        for (key, val) in &self.functions_name_map {
            if key.0 == main_name && key.1 != 0 {
                return Err(CompilerError {
                    message: "main function cannot have arguments".to_string(),
                    position: Some(val[0].0.args[0].2),
                });
            }
        }

        let keys = self.functions_name_map.keys().collect::<Vec<_>>().into_iter().cloned().collect::<Vec<_>>();
        for k in keys {
            let n = self.functions_name_map[&k].len();
            self.functions_specific_ordering.insert(k.clone(), Vec::new());
            let mut conns = HashSet::new();
            for i1 in 0..n {
                for i2 in 0..n {
                    if i1 == i2 {
                        continue;
                    }
                    let sign1 = self.functions_name_map[&k][i1].0.clone();
                    let sign2 = self.functions_name_map[&k][i2].0.clone();
                    if self.check_is_function_more_specific(&sign1, &sign2) {
                        self.functions_specific_ordering.get_mut(&k).unwrap().push((i1, i2));
                        conns.insert((i1, i2));
                        if conns.contains(&(i2, i1)) {
                            return Err(CompilerError {
                                message: format!("Found two equivalent signatures for function {}", k.0),
                                position: Some(self.functions_name_map[&k][i1].0.pos),
                            });
                        }
                    }
                }
            }
        }


        if let Some(mut vec) = self.functions_name_map.get(&(transform_function_name("main".to_string()), 0)).cloned() {
            if vec.len() != 1 {
                return Err(CompilerError {
                    message: "Multiple main functions found".to_string(),
                    position: None,
                });
            }

            let (sig, block) = vec.pop().unwrap();

            // normalize the main function (which recursively normalizes every instance that is used within
            // this is where vast majority of the work happens
            self.ir.main_function = self.normalize_function(sig, block, Vec::new(), Vec::new())?;
        } else {
            return Err(CompilerError {
                message: "No main function found".to_string(),
                position: None,
            });
        }

        // type deducer works alongside normalizer in the end we want to gather all types (which should be known by now)
        (self.ir.types, self.ir.autorefs) = self.type_resolver.gather_types(self.relevant_types)?;

        // check that main has no return value
        let main_ret = self.ir.instances[self.ir.main_function].ret_type;
        let main_ret = self.ir.types[&main_ret].clone();
        if main_ret != IRType::Primitive(IRPrimitiveType::Void) {
            return Err(CompilerError {
                message: "Main function should not return any value".to_string(),
                position: None,
            });
        }

        // so that functions are in right order, since labels are generated in increasing order
        // if func0 is used inside func1 it should appear above func1 in generated c code
        self.ir.instances.reverse();

        Ok(self.ir)
    }

    fn normalize_struct(&mut self, structure: ASTStructDeclaration) -> (IRStruct, Vec<ASTType>, Vec<(String, FilePosition)>) {
        let mut ir_struct = IRStruct { fields: Vec::new() };
        let mut type_hints = Vec::new();

        for (field_name, field_type) in structure.fields {
            let label = if let Some(label) = self.fields_name_map.get(&field_name) {
                *label
            } else {
                let label = self.new_field(&field_name);
                self.fields_name_map.insert(field_name, label);
                label
            };
            ir_struct.fields.push(label);
            type_hints.push(field_type);
        }

        (ir_struct, type_hints, structure.template)
    }

    fn gen_struct_field_types(&mut self, struct_label: IRStructLabel, template_args: Vec<ASTType>) -> CompilerResult<Vec<IRTypeLabel>> {
        let mut template_arg_labels = Vec::new();
        for arg in template_args {
            let pos = arg.get_pos();
            template_arg_labels.push((self.normalize_type(arg)?, pos));
        }

        let mut field_types = Vec::new();
        let mut old_template_types = HashMap::new();
        swap(&mut old_template_types, &mut self.template_types);
        let struct_template_names = self.structs_templates[struct_label].clone();
        let struct_fields = self.structs_type_hints[struct_label].clone();

        if template_arg_labels.len() > struct_template_names.len() {
            return Err(CompilerError {
                message: "Too many template arguments".to_string(),
                position: Some(template_arg_labels[struct_template_names.len()].1),
            });
        }

        for (i, (name, pos)) in struct_template_names.into_iter().enumerate() {
            let type_label = if let Some((type_label, _)) = template_arg_labels.get(i) {
                *type_label
            } else {
                self.type_resolver.new_type_label(pos)
            };
            self.template_types.insert(name, type_label);
        }

        for field in struct_fields {
            field_types.push(self.normalize_type(field)?);
        }

        swap(&mut old_template_types, &mut self.template_types);
        Ok(field_types)
    }

    fn find_matching_function(
        &mut self,
        function_name: &str,
        function_arguments: &[IRTypeLabel],
        template_arguments: &[IRTypeLabel],
        pos: FilePosition,
    ) -> CompilerResult<(ASTFunctionSignature, ASTBlock)> {
        let Some(candidates) = self.functions_name_map.get(&(function_name.to_string(), function_arguments.len())) else {
            return Err(CompilerError {
                message: format!("Function {} does not exist.", &function_name[1..]),
                position: Some(pos),
            });
        };
        let candidates = candidates.iter().map(|(sign, _block)| sign.clone()).collect::<Vec<_>>();

        let mut matching = HashSet::new();

        for (i, sign) in candidates.into_iter().enumerate() {
            if template_arguments.len() > sign.template.len() {
                continue;
            }

            let old_resolver = self.type_resolver.clone();
            let old_template_types = self.template_types.clone();
            self.template_types.clear();
            let mut ok = true;

            for (template_arg, pos) in sign.template.clone() {
                self.template_types.insert(template_arg, self.type_resolver.new_type_label(pos));
            }

            for (typ, (arg_name, _pos)) in template_arguments.iter().zip(&sign.template) {
                self.type_resolver.hint_equal(&self.ir, *typ, self.template_types[arg_name])?;
            }

            for ((_, hint, _), typ) in sign.args.iter().zip(function_arguments) {
                let hint_typ = self.normalize_type(hint.clone())?;

                if self.type_resolver.hint_equal(&self.ir, *typ, hint_typ).is_err() {
                    ok = false;
                    break;
                }
            }

            self.type_resolver = old_resolver;
            self.template_types = old_template_types;

            if ok {
                matching.insert(i);
            }
        }

        // eliminate more general functions
        let mut to_remove = HashSet::new();
        for (u, v) in &self.functions_specific_ordering[&(function_name.to_string(), function_arguments.len())] {
            if matching.contains(u) {
                to_remove.insert(*v);
            }
        }

        matching.retain(|x| !to_remove.contains(x));

        if matching.is_empty() {
            return Err(CompilerError {
                message: "No candidate found for this function call".to_string(),
                position: Some(pos),
            });
        }

        if matching.len() != 1 {
            return Err(CompilerError {
                message: "Multiple candidates found for this function call".to_string(),
                position: Some(pos),
            });
        }

        let mut idx = 0;
        for i in matching {
            idx = i;
        }
        Ok(self.functions_name_map[&(function_name.to_string(), function_arguments.len())][idx].clone())
    }

    fn normalize_expression(&mut self, expression: ASTExpression) -> CompilerResult<(IRExpression, IRTypeLabel, ValuePhysicality)> {
        let pos = expression.pos;
        let type_label = self.type_resolver.new_type_label(pos);
        let type_hint = self.normalize_type(expression.type_hint)?;
        self.type_resolver.hint_equal(&self.ir, type_label, type_hint)?;

        let (expr, is_phys) = match expression.kind {
            ASTExpressionKind::Integer(x) => {
                self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::I32)?;
                (
                    IRExpression::Constant {
                        constant: IRConstant::Int(i64::from(x)),
                    },
                    ValuePhysicality::Temporary,
                )
            }

            ASTExpressionKind::Float(x) => {
                self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::F32)?;
                (
                    IRExpression::Constant {
                        constant: IRConstant::Float(f64::from(x)),
                    },
                    ValuePhysicality::Temporary,
                )
            }

            ASTExpressionKind::String(x) => {
                self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::String)?;
                (
                    IRExpression::Constant {
                        constant: IRConstant::String(x),
                    },
                    ValuePhysicality::Temporary,
                )
            }

            ASTExpressionKind::Boolean(x) => {
                self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::Bool)?;
                (IRExpression::Constant { constant: IRConstant::Bool(x) }, ValuePhysicality::Temporary)
            }

            ASTExpressionKind::Variable(name) => {
                let label = *if let Some(label) = self.variables_name_map.get(&name) {
                    label
                } else {
                    return Err(CompilerError {
                        message: format!("Variable with name {name} not found."),
                        position: Some(pos),
                    });
                };
                let var_type_label = self.ir.variable_types[label];
                self.type_resolver.hint_equal(&self.ir, type_label, var_type_label)?;
                (IRExpression::Variable { variable_label: label }, ValuePhysicality::Physical)
            }

            ASTExpressionKind::Reference(expression) => {
                let (expression, type_label2, is_phys) = self.normalize_expression(*expression)?;

                if is_phys == ValuePhysicality::Temporary {
                    return Err(CompilerError {
                        message: "Cannot reference non-physical value.".to_string(),
                        position: Some(pos),
                    });
                }

                self.type_resolver.hint_is_ref(&self.ir, type_label2, type_label)?;
                (
                    IRExpression::Reference {
                        expression: Box::new(expression),
                    },
                    ValuePhysicality::Temporary,
                )
            }

            ASTExpressionKind::Dereference(expression) => {
                let (expression, type_label2, _is_phys) = self.normalize_expression(*expression)?;
                self.type_resolver.hint_is_ref(&self.ir, type_label, type_label2)?;
                (
                    IRExpression::Dereference {
                        expression: Box::new(expression),
                    },
                    ValuePhysicality::Physical,
                )
            }

            ASTExpressionKind::FunctionCall(call) => {
                let mut expr_types = Vec::new();
                let mut function_arguments = Vec::new();
                let mut template_types = Vec::new();

                let num_template_arguments = call.template_arguments.len();
                for typ in call.template_arguments {
                    let typ = self.normalize_type(typ)?;
                    template_types.push(typ);
                }

                for expr in call.arguments {
                    let (expr, type_label, _is_phys) = self.normalize_expression(expr)?;
                    expr_types.push(type_label);
                    function_arguments.push(expr);
                }

                if Self::is_builtin_function(&call.name) {
                    let (call, physicality, return_type) = self.get_builtin_call(call.name, expr_types, function_arguments, template_types, pos)?;
                    self.type_resolver.hint_equal(&self.ir, return_type, type_label)?;
                    (IRExpression::BuiltinFunctionCall(call), physicality)
                } else {
                    let (sig, block) = self.find_matching_function(&call.name, &expr_types, &template_types, pos)?;

                    if num_template_arguments > sig.num_template_args {
                        return Err(CompilerError {
                            message: format!(
                                "Too many template arguments. Got {} expected at most {}",
                                num_template_arguments, sig.num_template_args
                            ),
                            position: Some(pos),
                        });
                    }

                    let instance_label = self.normalize_function(sig, block, expr_types, template_types)?;
                    let ret_type = self.ir.instances[instance_label].ret_type;
                    self.type_resolver.hint_equal(&self.ir, ret_type, type_label)?;

                    (
                        IRExpression::InstanceCall {
                            instance_label,
                            instance_arguments: function_arguments,
                        },
                        ValuePhysicality::Temporary,
                    )
                }
            }

            ASTExpressionKind::StructInitialization {
                name,
                fields,
                template_arguments,
            } => {
                let struct_label = self.structs_name_map[&name];

                let field_type_labels = self.gen_struct_field_types(struct_label, template_arguments)?;

                let mut field_values = Vec::new();
                for (arg, field_type_label) in fields.into_iter().zip(field_type_labels.iter()) {
                    let (expr, typ, _is_phys) = self.normalize_expression(arg)?;
                    field_values.push(expr);
                    self.type_resolver.hint_equal(&self.ir, typ, *field_type_label)?;
                    self.relevant_types.push(*field_type_label);
                }

                let struct_expr = IRExpression::StructInitialization {
                    struct_label,
                    fields_type_labels: field_type_labels.clone(),
                    field_values,
                };
                self.type_resolver.hint_struct(&self.ir, type_label, struct_label, field_type_labels)?;

                (struct_expr, ValuePhysicality::Temporary)
            }

            ASTExpressionKind::FieldAccess { expression, field_name } => {
                let (expression, type_label2, _is_phys) = self.normalize_expression(*expression)?;
                let Some(&field_label) = self.fields_name_map.get(&field_name) else {
                    return Err(CompilerError {
                        message: format!("Unknown field {field_name}"),
                        position: Some(pos),
                    });
                };
                self.type_resolver.hint_is_field(&self.ir, type_label, type_label2, field_label)?;
                (
                    IRExpression::FieldAccess {
                        expression: Box::new(expression),
                        field_label,
                    },
                    ValuePhysicality::Physical,
                )
            }

            ASTExpressionKind::MethodCall { .. } => unreachable!("ASTExpression::MethodCall should be eliminated by lowerer"),

            ASTExpressionKind::BinaryOperation {
                expression1,
                operator,
                expression2,
            } => {
                let (expression1, type1_label, _is_phys) = self.normalize_expression(*expression1)?;
                let (expression2, type2_label, _is_phys) = self.normalize_expression(*expression2)?;
                self.relevant_types.push(type1_label);
                self.relevant_types.push(type2_label);
                self.type_resolver.hint_operator(&self.ir, type1_label, type2_label, operator, type_label)?;
                (
                    IRExpression::BinaryOperation {
                        operator,
                        expression1: Box::new(expression1),
                        expression2: Box::new(expression2),
                        type1_label,
                        type2_label,
                    },
                    ValuePhysicality::Temporary,
                )
            }

            ASTExpressionKind::AutoRef(expression) => {
                let (expression, type_label1, _is_phys) = self.normalize_expression(*expression)?;
                let autoref_label = self.type_resolver.new_autoref_label(type_label, type_label1);

                self.type_resolver.hint_autoref(&self.ir, type_label, type_label1)?;
                (
                    IRExpression::AutoRef {
                        autoref_label,
                        expression: Box::new(expression),
                    },
                    ValuePhysicality::Temporary,
                )
            }
        };

        Ok((expr, type_label, is_phys))
    }

    const fn primitive_type_to_ir_type(typ: ASTPrimitiveType) -> IRPrimitiveType {
        match typ {
            ASTPrimitiveType::I32 => IRPrimitiveType::I32,
            ASTPrimitiveType::I64 => IRPrimitiveType::I64,
            ASTPrimitiveType::F32 => IRPrimitiveType::F32,
            ASTPrimitiveType::F64 => IRPrimitiveType::F64,
            ASTPrimitiveType::Bool => IRPrimitiveType::Bool,
            ASTPrimitiveType::String => IRPrimitiveType::String,
            ASTPrimitiveType::Void => IRPrimitiveType::Void,
        }
    }

    fn normalize_type(&mut self, typ: ASTType) -> CompilerResult<IRTypeLabel> {
        let type_label = self.type_resolver.new_type_label(typ.get_pos());
        match typ {
            ASTType::Any(_) => {}
            ASTType::Primitive(typ, _) => {
                let typ = Self::primitive_type_to_ir_type(typ);
                self.type_resolver.hint_is(&self.ir, type_label, typ)?;
            }
            ASTType::Identifier(name, pos, template_args) => {
                if let Some(label) = self.template_types.get(&name).copied() {
                    // identifier is a template value
                    if !template_args.is_empty() {
                        return Err(CompilerError {
                            message: "Template value cannot have template arguments".to_string(),
                            position: Some(pos),
                        });
                    }
                    self.type_resolver.hint_equal(&self.ir, label, type_label)?;
                } else if let Some(struct_label) = self.structs_name_map.get(&name).copied() {
                    // identifier is a concrete struct
                    let args = self.gen_struct_field_types(struct_label, template_args)?;

                    self.type_resolver.hint_struct(&self.ir, type_label, struct_label, args)?;
                } else {
                    return Err(CompilerError {
                        message: format!("Unknown type: {name}"),
                        position: Some(pos),
                    });
                }
            }
            ASTType::Reference(typ, _) => {
                let type_label2 = self.normalize_type(*typ)?;
                self.type_resolver.hint_is_ref(&self.ir, type_label2, type_label)?;
            }
        }
        Ok(type_label)
    }

    fn normalize_block(&mut self, block: ASTBlock) -> CompilerResult<IRBlock> {
        let mut res = IRBlock { statements: Vec::new() };
        let prev_vars = self.variables_name_map.clone();

        for statement in block.children {
            match statement {
                ASTStatement::Assignment { assign_to, value, pos } => {
                    // if an unknown variable is assigned, create it
                    if let ASTExpressionKind::Variable(name) = &assign_to.kind
                        && !self.variables_name_map.contains_key(name)
                    {
                        let label = self.new_var(name);
                        self.curr_func_vars.push(label);
                        let type_label = self.type_resolver.new_type_label(assign_to.pos);
                        self.ir.variable_types.push(type_label);
                        self.relevant_types.push(type_label);
                    }

                    let (assign_to, type_label1, is_phys) = self.normalize_expression(assign_to)?;
                    let (value, type_label2, _is_phys) = self.normalize_expression(value)?;
                    self.type_resolver.hint_equal(&self.ir, type_label1, type_label2)?;

                    if is_phys == ValuePhysicality::Temporary {
                        return Err(CompilerError {
                            message: "Left hand side is non-assignable".to_string(),
                            position: Some(pos),
                        });
                    }

                    res.statements.push(IRStatement::Assignment { assign_to, value });
                }
                ASTStatement::AssignmentOperator { .. } | ASTStatement::AssignmentIncrement { .. } | ASTStatement::AssignmentDecrement { .. } => unreachable!(), // lowerer took care of that

                ASTStatement::Block { block } => {
                    let block = self.normalize_block(block)?;
                    res.statements.push(IRStatement::Block { block });
                }
                ASTStatement::Expression { expression } => {
                    res.statements.push(IRStatement::Expression {
                        expr: self.normalize_expression(expression)?.0,
                    });
                }
                ASTStatement::Print { values } => {
                    let mut vals = values;
                    vals.push(ASTExpression::no_hint(ASTExpressionKind::String("\n".to_string()), FilePosition::unknown()));
                    for val in vals {
                        let (expr, type_label, _is_phys) = self.normalize_expression(val)?;
                        self.relevant_types.push(type_label);
                        res.statements.push(IRStatement::Print { expr, type_label });
                    }
                }
                ASTStatement::Return { return_value, pos: _ } => {
                    self.has_ret_statement = true;
                    let st = if let Some(expr) = return_value {
                        let (expr, type_label, _is_phys) = self.normalize_expression(expr)?;
                        self.type_resolver.hint_equal(&self.ir, self.curr_func_ret_type, type_label)?;
                        IRStatement::Return { return_value: Some(expr) }
                    } else {
                        self.type_resolver.hint_is(&self.ir, self.curr_func_ret_type, IRPrimitiveType::Void)?;
                        IRStatement::Return { return_value: None }
                    };
                    res.statements.push(st);
                }
                ASTStatement::If { condition, block, else_block } => {
                    let (condition, type_label, _is_phys) = self.normalize_expression(condition)?;
                    self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::Bool)?;
                    let block = self.normalize_block(block)?;
                    let else_block = if let Some(else_block) = else_block {
                        Some(self.normalize_block(else_block)?)
                    } else {
                        None
                    };
                    res.statements.push(IRStatement::If { condition, block, else_block });
                }
                ASTStatement::While { condition, block } => {
                    let (condition, type_label, _is_phys) = self.normalize_expression(condition)?;
                    self.type_resolver.hint_is(&self.ir, type_label, IRPrimitiveType::Bool)?;
                    let block = self.normalize_block(block)?;
                    res.statements.push(IRStatement::While { condition, block });
                }
            }
        }

        self.variables_name_map = prev_vars;

        Ok(res)
    }

    fn check_instance_cache(&mut self, function_name: &String, arg_types: &Vec<IRTypeLabel>) -> Option<IRInstanceLabel> {
        let cache = self.instance_cache.get(function_name)?;
        for (types, label) in cache {
            if types.len() != arg_types.len() {
                continue;
            }

            let mut ok = true;
            for (t1, t2) in types.iter().zip(arg_types) {
                if !self.type_resolver.are_equal(*t1, *t2) {
                    ok = false;
                    break;
                }
            }

            if ok {
                return Some(*label);
            }
        }
        None
    }

    fn normalize_function(
        &mut self,
        sign: ASTFunctionSignature,
        block: ASTBlock,
        arg_types: Vec<IRTypeLabel>,
        template_types: Vec<IRTypeLabel>,
    ) -> CompilerResult<IRInstanceLabel> {
        const RECURSION_LIMIT: i32 = 100;
        assert_eq!(arg_types.len(), sign.args.len());

        // check if function has been already normalized
        // this not only improves performance and makes generated code smaller,
        // it also enables recursion
        if let Some(label) = self.check_instance_cache(&sign.name, &arg_types) {
            return Ok(label);
        }

        for type_label in &arg_types {
            self.relevant_types.push(*type_label);
        }

        // backup and override values that are needed by instance normalizing
        let prev_func_vars = self.curr_func_vars.clone();
        let prev_func_ret_type = self.curr_func_ret_type;
        let prev_has_ret_statement = self.has_ret_statement;
        let mut old_variables_name_map = HashMap::new();
        swap(&mut old_variables_name_map, &mut self.variables_name_map);

        if self.depth == RECURSION_LIMIT {
            return Err(CompilerError {
                message: format!(
                    "This function is in the {RECURSION_LIMIT}-th recursive call \
            in the normalization phase. Make sure argument types are more \
            explicit and the function does not generate infinitely many functions recursively."
                ),
                position: Some(sign.pos),
            });
        }

        self.depth += 1;
        self.curr_func_vars = Vec::new();
        self.curr_func_ret_type = self.type_resolver.new_type_label(sign.pos);
        self.relevant_types.push(self.curr_func_ret_type);
        self.has_ret_statement = false;
        let label = self.curr_func_label;
        self.curr_func_label += 1;

        for (idx, (template_arg, pos)) in sign.template.into_iter().enumerate() {
            let typ = if let Some(template_arg) = template_types.get(idx) {
                *template_arg
            } else {
                self.type_resolver.new_type_label(pos)
            };
            self.template_types.insert(template_arg, typ);
        }

        let mut arguments = Vec::new();
        for ((arg, type_hint, _pos), arg_type) in sign.args.into_iter().zip(arg_types.clone()) {
            let hint_label = self.normalize_type(type_hint)?;
            let label = self.new_var(&arg);
            arguments.push(label);
            self.ir.variable_types.push(arg_type);
            self.type_resolver.hint_equal(&self.ir, hint_label, arg_type)?;
        }

        self.ir.instances.push(IRInstance {
            arguments,
            variables: Vec::new(),
            ret_type: self.curr_func_ret_type,
            block: IRBlock { statements: Vec::new() },
            label,
        });

        if !self.instance_cache.contains_key(&sign.name) {
            self.instance_cache.insert(sign.name.clone(), Vec::new());
        }

        self.instance_cache.get_mut(&sign.name).unwrap().push((arg_types, label));

        self.ir.instances[label].block = self.normalize_block(block)?;
        self.ir.instances[label].variables = self.curr_func_vars.clone();

        if !self.has_ret_statement {
            self.type_resolver.hint_is(&self.ir, self.curr_func_ret_type, IRPrimitiveType::Void)?;
        }

        self.curr_func_vars = prev_func_vars;
        self.curr_func_ret_type = prev_func_ret_type;
        self.has_ret_statement = prev_has_ret_statement;
        self.template_types.clear();
        swap(&mut old_variables_name_map, &mut self.variables_name_map);
        self.depth -= 1;

        Ok(label)
    }
}
