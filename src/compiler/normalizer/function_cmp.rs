use crate::compiler::error::{CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::IRTypeLabel;
use crate::compiler::normalizer::Normalizer;
use crate::compiler::parser::ast::ASTFunctionSignature;
use crate::compiler::type_resolver::TypeResolver;
use std::collections::HashMap;
use std::mem::swap;

impl Normalizer {
    pub fn add_func_to_resolver(&mut self, func: &ASTFunctionSignature) -> CompilerResult<Vec<IRTypeLabel>> {
        let mut old_template_types = HashMap::new();
        // this is ugly (might refactor later)
        swap(&mut old_template_types, &mut self.template_types);

        for (template_arg, _pos) in &func.template {
            let typ = self.type_resolver.new_type_label(FilePosition::unknown());
            self.template_types.insert(template_arg.clone(), typ);
        }

        let mut arg_types = Vec::new();
        for (_arg, type_hint, _pos) in &func.args {
            arg_types.push(self.normalize_type(type_hint.clone())?);
        }

        swap(&mut old_template_types, &mut self.template_types);
        Ok(arg_types)
    }

    // is func1 more specific than func2 - so every call that satisfies func1 also satisfies func2
    pub fn check_is_function_more_specific(&mut self, func1: &ASTFunctionSignature, func2: &ASTFunctionSignature) -> bool {
        let mut old_resolver = TypeResolver::new();
        // this is ugly (might refactor later)
        swap(&mut old_resolver, &mut self.type_resolver);

        let Ok(args1) = self.add_func_to_resolver(func1) else { return false };
        let Ok(args2) = self.add_func_to_resolver(func1) else { return false };
        let Ok(args3) = self.add_func_to_resolver(func2) else { return false };

        for (arg1, arg2) in args2.iter().zip(args3) {
            if self.type_resolver.hint_equal(&self.ir, *arg1, arg2).is_err() {
                return false;
            }
        }

        let res = self.type_resolver.compare_sets(args1, args2);
        swap(&mut old_resolver, &mut self.type_resolver);
        res
    }
}