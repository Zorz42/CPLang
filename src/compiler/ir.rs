type IRFunctionLabel = usize;
type IRVariableLabel = usize;

enum IRPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

enum IRType {
    Primitive(IRPrimitiveType),
    Reference(Box<IRType>),
    Tuple(Vec<IRType>),
    Enum(Vec<IRType>),
}

enum IROperator {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Greater,
    GreaterOrEq,
    Lesser,
    LesserOrEq,
}

enum IRConstant {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

enum IRExpression {
    Operator(IROperator, Box<(IRExpression, IRExpression)>),
    Constant(IRConstant),
    FunctionCall(IRFunctionLabel, Box<IRExpression>),
}