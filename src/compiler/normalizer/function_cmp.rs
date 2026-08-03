use crate::compiler::error::{CompilerResult, FilePosition};
use crate::compiler::normalizer::Normalizer;
use crate::compiler::normalizer::ir::IRTypeLabel;
use crate::compiler::parser::ast::ASTFunctionSignature;
use crate::compiler::type_resolver::rollback::Rollback;
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
    pub fn check_is_function_more_specific(&mut self, func1_args1: Vec<IRTypeLabel>, func1_args2: Vec<IRTypeLabel>, func2_args: Vec<IRTypeLabel>) -> bool {
        #[cfg(feature = "trace")]
        {
            println!("===============");
            println!("Comparing");
        }

        let state = self.type_resolver.save_state();

        for (arg1, arg2) in func1_args2.iter().zip(func2_args) {
            if self.type_resolver.hint_equal(*arg1, arg2).is_err() {
                self.type_resolver.restore_state(state);
                #[cfg(feature = "trace")]
                {
                    println!("Verdict: false");
                    println!("===============");
                }

                return false;
            }
        }

        let res = self.type_resolver.compare_sets(func1_args1, func1_args2);
        self.type_resolver.restore_state(state);
        #[cfg(feature = "trace")]
        {
            println!("Verdict: {res}");
            println!("===============");
        }
        res
    }
}
