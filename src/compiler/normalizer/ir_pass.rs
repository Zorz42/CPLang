use crate::compiler::normalizer::ir::{IR, IRBlock, IRBuiltinFunctionCall, IRExpression, IRInstance, IRStatement, IRStruct};

pub trait IRPass {
    // methods to be overridden
    fn map_ir(&mut self, ir: IR) -> IR {
        ir
    }
    fn map_struct(&mut self, structure: IRStruct) -> IRStruct {
        structure
    }
    fn pre_map_instance(&mut self, instance: IRInstance) -> IRInstance {
        instance
    }
    fn post_map_instance(&mut self, instance: IRInstance) -> IRInstance {
        instance
    }
    fn pre_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        statement
    }
    fn post_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        statement
    }
    fn pre_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        expression
    }
    fn post_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        expression
    }

    // internal methods
    fn pass_ir(&mut self, ir: IR) -> IR {
        let mut ir = self.map_ir(ir);
        ir.structs = ir.structs.into_iter().map(|x| self.map_struct(x)).collect();
        ir.instances = ir
            .instances
            .into_iter()
            .map(|instance| {
                let mut instance = self.pre_map_instance(instance);
                instance.block = self.pass_block(instance.block);
                self.post_map_instance(instance)
            })
            .collect();
        ir
    }

    fn pass_block(&mut self, mut block: IRBlock) -> IRBlock {
        block.statements = block
            .statements
            .into_iter()
            .map(|statement| {
                let statement = self.pre_map_statement(statement);
                let statement = match statement {
                    IRStatement::Assignment { assign_to, value, pos } => IRStatement::Assignment {
                        assign_to: self.pass_expression(assign_to),
                        value: self.pass_expression(value),
                        pos,
                    },
                    IRStatement::If { condition, block, else_block } => IRStatement::If {
                        condition: self.pass_expression(condition),
                        block: self.pass_block(block),
                        else_block: else_block.map(|block| self.pass_block(block)),
                    },
                    IRStatement::While { condition, block } => IRStatement::While {
                        condition: self.pass_expression(condition),
                        block: self.pass_block(block),
                    },
                    IRStatement::Block { block } => IRStatement::Block { block: self.pass_block(block) },
                    IRStatement::Expression { expr } => IRStatement::Expression {
                        expr: self.pass_expression(expr),
                    },
                    IRStatement::Return { return_value } => IRStatement::Return {
                        return_value: return_value.map(|expr| self.pass_expression(expr)),
                    },
                    IRStatement::Break { .. } | IRStatement::Continue { .. } => statement,
                };
                self.post_map_statement(statement)
            })
            .collect();
        block
    }

    fn pass_expression(&mut self, expression: IRExpression) -> IRExpression {
        let expression = self.pre_map_expression(expression);
        let expression = match expression {
            IRExpression::AutoRef { autoref_label, expression } => IRExpression::AutoRef {
                autoref_label,
                expression: Box::new(self.pass_expression(*expression)),
            },
            IRExpression::BuiltinFunctionCall(call) => IRExpression::BuiltinFunctionCall(self.pass_builtin_call(call)),
            IRExpression::Constant { constant } => IRExpression::Constant { constant },
            IRExpression::Dereference { expression } => IRExpression::Dereference {
                expression: Box::new(self.pass_expression(*expression)),
            },
            IRExpression::FieldAccess { expression, field_label } => IRExpression::FieldAccess {
                expression: Box::new(self.pass_expression(*expression)),
                field_label,
            },
            IRExpression::InstanceCall {
                instance_label,
                instance_arguments,
            } => IRExpression::InstanceCall {
                instance_label,
                instance_arguments: instance_arguments.into_iter().map(|expr| self.pass_expression(expr)).collect(),
            },
            IRExpression::StructInitialization {
                struct_label,
                fields_type_labels,
                field_values,
            } => IRExpression::StructInitialization {
                struct_label,
                fields_type_labels,
                field_values: field_values.into_iter().map(|expr| self.pass_expression(expr)).collect(),
            },
            IRExpression::Reference { expression, pos } => IRExpression::Reference {
                expression: Box::new(self.pass_expression(*expression)),
                pos,
            },
            IRExpression::Variable { variable_label } => IRExpression::Variable { variable_label },
        };
        self.post_map_expression(expression)
    }

    fn pass_builtin_call(&mut self, call: IRBuiltinFunctionCall) -> IRBuiltinFunctionCall {
        match call {
            IRBuiltinFunctionCall::Alloc { typ, num } => IRBuiltinFunctionCall::Alloc {
                num: Box::new(self.pass_expression(*num)),
                typ,
            },
            IRBuiltinFunctionCall::Index { arr, idx } => IRBuiltinFunctionCall::Index {
                arr: Box::new(self.pass_expression(*arr)),
                idx: Box::new(self.pass_expression(*idx)),
            },
            IRBuiltinFunctionCall::IndexStr { string, idx } => IRBuiltinFunctionCall::IndexStr {
                string: Box::new(self.pass_expression(*string)),
                idx: Box::new(self.pass_expression(*idx)),
            },
            IRBuiltinFunctionCall::Getchar {} => call,
            IRBuiltinFunctionCall::Putchar { arg } => IRBuiltinFunctionCall::Putchar {
                arg: Box::new(self.pass_expression(*arg)),
            },
            IRBuiltinFunctionCall::Cast { arg, to_type } => IRBuiltinFunctionCall::Cast {
                arg: Box::new(self.pass_expression(*arg)),
                to_type,
            },
            IRBuiltinFunctionCall::Add { arg1, arg2 } => IRBuiltinFunctionCall::Add {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Sub { arg1, arg2 } => IRBuiltinFunctionCall::Sub {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Mul { arg1, arg2 } => IRBuiltinFunctionCall::Mul {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Div { arg1, arg2 } => IRBuiltinFunctionCall::Div {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Mod { arg1, arg2 } => IRBuiltinFunctionCall::Mod {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Eq { arg1, arg2 } => IRBuiltinFunctionCall::Eq {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::NotEq { arg1, arg2 } => IRBuiltinFunctionCall::NotEq {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Lesser { arg1, arg2 } => IRBuiltinFunctionCall::Lesser {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Greater { arg1, arg2 } => IRBuiltinFunctionCall::Greater {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::LesserEq { arg1, arg2 } => IRBuiltinFunctionCall::LesserEq {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::GreaterEq { arg1, arg2 } => IRBuiltinFunctionCall::GreaterEq {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::And { arg1, arg2 } => IRBuiltinFunctionCall::And {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Or { arg1, arg2 } => IRBuiltinFunctionCall::Or {
                arg1: Box::new(self.pass_expression(*arg1)),
                arg2: Box::new(self.pass_expression(*arg2)),
            },
            IRBuiltinFunctionCall::Not { arg } => IRBuiltinFunctionCall::Not {
                arg: Box::new(self.pass_expression(*arg)),
            },
        }
    }
}
