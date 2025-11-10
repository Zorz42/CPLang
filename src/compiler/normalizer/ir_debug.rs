use crate::compiler::normalizer::ir::{IR, IRBlock, IRFunction};
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

impl Debug for IRBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IRBlock")?;
        writeln!(f, "Statements:")?;
        let mut output = String::new();
        for statement in &self.statements {
            writeln!(&mut output, "{:?}", statement)?;
        }
        writeln!(f, "    {}", output.replace("\n", "\n    "))?;
        Ok(())
    }
}

impl Debug for IRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Arguments: {:?}", self.arguments)?;
        writeln!(f, "Variables: {:?}", self.variables)?;
        write!(f, "{:?}", self.block)?;
        Ok(())
    }
}

impl Debug for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Structs: ")?;
        let mut output = String::new();
        for statement in &self.structs {
            writeln!(&mut output, "{:?}", statement)?;
        }
        writeln!(f, "    {}", output.replace("\n", "\n    "))?;
        writeln!(f, "Functions: ")?;
        let mut output = String::new();
        for statement in &self.functions {
            writeln!(&mut output, "{:?}", statement)?;
        }
        writeln!(f, "    {}", output.replace("\n", "\n    "))?;
        Ok(())
    }
}
