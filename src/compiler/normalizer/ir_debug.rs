use crate::compiler::normalizer::ir::{IRBlock, IRInstance, IRPrimitiveType, IRType, IR};
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

impl Debug for IRBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IRBlock")?;
        writeln!(f, "Statements:")?;
        let mut output = String::new();
        for statement in &self.statements {
            writeln!(&mut output, "{statement:?}")?;
        }
        writeln!(f, "    {}", output.replace('\n', "\n    "))?;
        Ok(())
    }
}

impl Debug for IRInstance {
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
            writeln!(&mut output, "{statement:?}")?;
        }
        writeln!(f, "    {}", output.replace('\n', "\n    "))?;
        writeln!(f, "Functions: ")?;
        let mut output = String::new();
        for statement in &self.instances {
            writeln!(&mut output, "{statement:?}")?;
        }
        writeln!(f, "    {}", output.replace('\n', "\n    "))?;
        Ok(())
    }
}

impl Debug for IRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(prim) => {
                let string = match prim {
                    IRPrimitiveType::I32 => "i32",
                    IRPrimitiveType::I64 => "i64",
                    IRPrimitiveType::F32 => "f32",
                    IRPrimitiveType::F64 => "f64",
                    IRPrimitiveType::Bool => "bool",
                    IRPrimitiveType::String => "string",
                    IRPrimitiveType::Void => "()",
                };
                write!(f, "{string}")?;
            }
            Self::Reference(typ) => write!(f, "&{:?}", *typ)?,
            Self::Struct(label, types) => {
                write!(f, "S{label}")?;
                let n = types.len();
                if n != 0 {
                    write!(f, "<")?;
                    for (i, typ) in types.iter().enumerate() {
                        write!(f, "{typ:?}")?;
                        if i != n - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ">")?;
                }
            }
        }

        Ok(())
    }
}