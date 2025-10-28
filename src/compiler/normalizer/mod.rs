use std::collections::HashMap;
use crate::compiler::error::CompilerResult;
use crate::compiler::normalizer::ir::{IRBlock, IRFunction, IRStatement, IRStruct, IRVariableLabel};
use crate::compiler::parser::{Statement, AST};
use crate::compiler::parser::block::Block;

mod ir;

#[derive(Debug)]
pub struct IR {
    structs: Vec<IRStruct>,
    functions: Vec<IRFunction>,
}

pub struct NormalizerState {
    variables: HashMap<String, IRVariableLabel>,
}

pub fn normalize(ast: &AST) -> CompilerResult<IR> {
    let mut res = IR {
        structs: Vec::new(),
        functions: Vec::new(),
    };
    let mut state = NormalizerState {
        variables: HashMap::new(),
    };

    for (sig, block) in &ast.functions {
        let block = normalize_block(&mut state, block);
        res.functions.push(IRFunction {
            arguments: Vec::new(),
            block,
        });
    }

    Ok(res)
}

fn normalize_block(state: &mut NormalizerState, block: &Block) -> IRBlock {
    let mut res = IRBlock {
        variables: Vec::new(),
        statements: Vec::new(),
    };

    for statement in &block.children {
        match statement {
            Statement::VariableDeclaration(declaration) => {

            }
            Statement::Block(block) => {
                let block = normalize_block(state, block);
                res.statements.push(IRStatement::Block(block));
            }
            Statement::Expression(_) => {}
            Statement::Print(_) => {}
            Statement::Return(_, _) => {}
            Statement::If(_) => {}
            Statement::While(_) => {}
        }
    }

    res
}