use crate::compiler::parser::function::parse_function_declaration;
use crate::compiler::tokenizer::Block;

mod function;

pub fn parse_tokens(program_block: &Block) {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    while curr_idx < program_block.children.len() {
        let declaration = parse_function_declaration(&program_block, &mut curr_idx);
        function_declarations.push(declaration);
    }

    println!("{:?}", function_declarations);
}