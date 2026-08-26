use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTGlobalVariable, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type_hint;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, assign_to: ASTExpression, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    let (token, _) = block.peek();
    match token {
        Token::Assign => {
            block.get();
        }
        _ => return Ok(None),
    }

    let assign_to_pos = assign_to.pos;

    let res = {
        let value = parse_expression(structs, block)?;
        let pos = assign_to_pos + value.pos;
        ASTStatement::Assignment { assign_to, value, pos }
    };

    Ok(Some(res))
}

pub fn parse_global_variable_declaration(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock, file_idx: usize) -> CompilerResult<ASTGlobalVariable> {
    let (ident, ident_pos) = match block.get() {
        (Token::Identifier(ident), ident_pos) => (ident, ident_pos),
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token, expected one of: fn, struct, identifier".to_owned(),
                position: Some(pos),
            });
        }
    };

    let hint = parse_type_hint(block)?;

    let value = if let (Token::Assign, _) = block.peek() {
        block.get();
        let value = parse_expression(structs, block)?;
        Some(value)
    } else {
        None
    };

    Ok(ASTGlobalVariable {
        name: ident,
        type_hint: hint,
        initial_value: value,
        pos: ident_pos,
        file_idx,
    })
}
