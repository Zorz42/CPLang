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

    #[test]
    fn test_hint_multiple_primitive() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ1, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::F64).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Void).unwrap();
        resolver.hint_is(typ4, IRPrimitiveType::String).unwrap();
        let (types, _) = resolver.gather_types(vec![typ1, typ2, typ3, typ4]).unwrap();
        assert_eq!(types[&typ1], IRType::Primitive(IRPrimitiveType::I32));
        assert_eq!(types[&typ2], IRType::Primitive(IRPrimitiveType::F64));
        assert_eq!(types[&typ3], IRType::Primitive(IRPrimitiveType::Void));
        assert_eq!(types[&typ4], IRType::Primitive(IRPrimitiveType::String));
    }

    #[test]
    fn test_hint_is() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        let typ6 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_equal(typ1, typ2).unwrap();
        resolver.hint_equal(typ3, typ4).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::Void).unwrap();
        resolver.hint_equal(typ1, typ3).unwrap();
        resolver.hint_equal(typ5, typ6).unwrap();
        resolver.hint_is(typ5, IRPrimitiveType::String).unwrap();
        let (types, _) = resolver.gather_types(vec![typ4, typ6]).unwrap();
        assert_eq!(types[&typ4], IRType::Primitive(IRPrimitiveType::Void));
        assert_eq!(types[&typ6], IRType::Primitive(IRPrimitiveType::String));
    }
}