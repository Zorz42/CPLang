#[cfg(test)]
mod test_type_resolver {
    use crate::compiler::error::FilePosition;
    use crate::compiler::normalizer::ir::{IRPrimitiveType, IRType};
    use crate::compiler::type_resolver::TypeResolver;

    #[test]
    fn test_new() {
        let _ = TypeResolver::new(Vec::new());
    }

    #[test]
    fn test_hint_primitive() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ, IRPrimitiveType::I32).unwrap();
        let (types, _) = resolver.gather_types(vec![typ]).unwrap();
        assert_eq!(types[&typ], IRType::Primitive(IRPrimitiveType::I32));
    }
}