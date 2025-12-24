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

    #[test]
    fn test_are_equal() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        let typ6 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_equal(typ1, typ2).unwrap();
        assert!(resolver.are_equal(typ1, typ2));
        assert!(!resolver.are_equal(typ1, typ3));
        resolver.hint_equal(typ3, typ4).unwrap();
        assert!(!resolver.are_equal(typ1, typ3));
        assert!(resolver.are_equal(typ1, typ2));
        assert!(resolver.are_equal(typ3, typ4));
        resolver.hint_is(typ2, IRPrimitiveType::String).unwrap();
        resolver.hint_equal(typ1, typ3).unwrap();
        assert!(resolver.are_equal(typ1, typ3));
        assert!(resolver.are_equal(typ2, typ4));
        resolver.hint_equal(typ5, typ6).unwrap();
        assert!(!resolver.are_equal(typ2, typ6));
        resolver.hint_is(typ5, IRPrimitiveType::String).unwrap();
        assert!(resolver.are_equal(typ2, typ6));
    }

    #[test]
    fn test_ref() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is_ref(typ1, typ2).unwrap();
        resolver.hint_is(typ1, IRPrimitiveType::Bool).unwrap();
        let (res, _) = resolver.gather_types(vec![typ2]).unwrap();
        assert_eq!(res[&typ2], IRType::Reference(Box::new(IRType::Primitive(IRPrimitiveType::Bool))));
    }

    #[test]
    fn test_struct() {
        let mut resolver = TypeResolver::new(vec![vec![0, 1]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2, typ3]).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Void).unwrap();
        let (res, _) = resolver.gather_types(vec![typ1]).unwrap();
        assert_eq!(res[&typ1], IRType::Struct(0, vec![IRType::Primitive(IRPrimitiveType::I32), IRType::Primitive(IRPrimitiveType::Void)]));
    }

    #[test]
    fn test_struct_is_eq() {
        let mut resolver = TypeResolver::new(vec![vec![0, 1]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        let typ6 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2, typ3]).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ6, IRPrimitiveType::Void).unwrap();
        assert!(resolver.are_equal(typ1, typ4));
    }

    #[test]
    fn test_struct_is_not_eq_field() {
        let mut resolver = TypeResolver::new(vec![vec![0, 1]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        let typ6 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2, typ3]).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, IRPrimitiveType::Void).unwrap();
        resolver.hint_is(typ6, IRPrimitiveType::I32).unwrap();
        assert!(!resolver.are_equal(typ1, typ4));
    }

    #[test]
    fn test_struct_is_not_eq_struct_type() {
        let mut resolver = TypeResolver::new(vec![vec![0, 1], vec![0, 1]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        let typ6 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2, typ3]).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 1, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, IRPrimitiveType::I32).unwrap();
        resolver.hint_is(typ6, IRPrimitiveType::Void).unwrap();
        assert!(!resolver.are_equal(typ1, typ4));
    }

    #[test]
    fn test_hint_field() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2]).unwrap();
        resolver.hint_is_field(typ3, typ1, 0).unwrap();
        resolver.hint_is(typ3, IRPrimitiveType::Bool).unwrap();
        assert_eq!(resolver.fetch_final_ir_type(typ1), Some(IRType::Struct(0, vec![IRType::Primitive(IRPrimitiveType::Bool)])));
    }

    #[test]
    fn test_hint_field_reffed() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ4, 0, vec![typ1]).unwrap();
        resolver.hint_is_field(typ3, typ4, 0).unwrap();
        assert!(resolver.are_equal(typ1, typ3));
        resolver.hint_is_ref(typ2, typ3).unwrap();
        resolver.hint_is(typ2, IRPrimitiveType::String).unwrap();
        assert_eq!(resolver.fetch_final_ir_type(typ4), Some(IRType::Struct(0, vec![
            IRType::Reference(Box::new(IRType::Primitive(IRPrimitiveType::String)))
        ])));
    }
}