#[cfg(test)]
mod test_type_resolver {
    use crate::compiler::error::FilePosition;
    use crate::compiler::normalizer::ir::IRType;
    use crate::compiler::parser::ast::PrimitiveType;
    use crate::compiler::type_resolver::TypeResolver;

    #[test]
    fn test_new() {
        let _ = TypeResolver::new(Vec::new());
    }

    #[test]
    fn test_hint_primitive() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ, PrimitiveType::I32).unwrap();
        let (types, _) = resolver.gather_types(vec![typ]).unwrap();
        assert_eq!(types[&typ], IRType::Primitive(PrimitiveType::I32));
    }

    #[test]
    fn test_hint_multiple_primitive() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ1, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ2, PrimitiveType::F64).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Void).unwrap();
        resolver.hint_is(typ4, PrimitiveType::String).unwrap();
        let (types, _) = resolver.gather_types(vec![typ1, typ2, typ3, typ4]).unwrap();
        assert_eq!(types[&typ1], IRType::Primitive(PrimitiveType::I32));
        assert_eq!(types[&typ2], IRType::Primitive(PrimitiveType::F64));
        assert_eq!(types[&typ3], IRType::Primitive(PrimitiveType::Void));
        assert_eq!(types[&typ4], IRType::Primitive(PrimitiveType::String));
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
        resolver.hint_is(typ2, PrimitiveType::Void).unwrap();
        resolver.hint_equal(typ1, typ3).unwrap();
        resolver.hint_equal(typ5, typ6).unwrap();
        resolver.hint_is(typ5, PrimitiveType::String).unwrap();
        let (types, _) = resolver.gather_types(vec![typ4, typ6]).unwrap();
        assert_eq!(types[&typ4], IRType::Primitive(PrimitiveType::Void));
        assert_eq!(types[&typ6], IRType::Primitive(PrimitiveType::String));
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
        resolver.hint_is(typ2, PrimitiveType::String).unwrap();
        resolver.hint_equal(typ1, typ3).unwrap();
        assert!(resolver.are_equal(typ1, typ3));
        assert!(resolver.are_equal(typ2, typ4));
        resolver.hint_equal(typ5, typ6).unwrap();
        assert!(!resolver.are_equal(typ2, typ6));
        resolver.hint_is(typ5, PrimitiveType::String).unwrap();
        assert!(resolver.are_equal(typ2, typ6));
    }

    #[test]
    fn test_ref() {
        let mut resolver = TypeResolver::new(Vec::new());
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is_ref(typ1, typ2).unwrap();
        resolver.hint_is(typ1, PrimitiveType::Bool).unwrap();
        let (res, _) = resolver.gather_types(vec![typ2]).unwrap();
        assert_eq!(res[&typ2], IRType::Reference(Box::new(IRType::Primitive(PrimitiveType::Bool))));
    }

    #[test]
    fn test_struct() {
        let mut resolver = TypeResolver::new(vec![vec![0, 1]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2, typ3]).unwrap();
        resolver.hint_is(typ2, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Void).unwrap();
        let (res, _) = resolver.gather_types(vec![typ1]).unwrap();
        assert_eq!(
            res[&typ1],
            IRType::Struct(0, vec![IRType::Primitive(PrimitiveType::I32), IRType::Primitive(PrimitiveType::Void)])
        );
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
        resolver.hint_is(typ2, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ6, PrimitiveType::Void).unwrap();
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
        resolver.hint_is(typ2, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, PrimitiveType::Void).unwrap();
        resolver.hint_is(typ6, PrimitiveType::I32).unwrap();
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
        resolver.hint_is(typ2, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Void).unwrap();
        resolver.hint_struct(typ4, 1, vec![typ5, typ6]).unwrap();
        resolver.hint_is(typ5, PrimitiveType::I32).unwrap();
        resolver.hint_is(typ6, PrimitiveType::Void).unwrap();
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
        resolver.hint_is(typ3, PrimitiveType::Bool).unwrap();
        assert_eq!(
            resolver.fetch_final_ir_type(typ1),
            Some(IRType::Struct(0, vec![IRType::Primitive(PrimitiveType::Bool)]))
        );
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
        resolver.hint_is(typ2, PrimitiveType::String).unwrap();
        assert_eq!(
            resolver.fetch_final_ir_type(typ4),
            Some(IRType::Struct(0, vec![IRType::Reference(Box::new(IRType::Primitive(PrimitiveType::String)))]))
        );
    }

    #[test]
    fn test_equal_struct() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2]).unwrap();
        resolver.hint_is_field(typ3, typ1, 0).unwrap();
        resolver.hint_is(typ3, PrimitiveType::Bool).unwrap();
        resolver.hint_equal(typ1, typ4).unwrap();
        assert_eq!(
            resolver.fetch_final_ir_type(typ4),
            Some(IRType::Struct(0, vec![IRType::Primitive(PrimitiveType::Bool)]))
        );
    }

    #[test]
    fn test_deduce_field() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_struct(typ1, 0, vec![typ2]).unwrap();
        resolver.hint_is(typ2, PrimitiveType::Bool).unwrap();
        resolver.hint_autoref(typ3, typ1).unwrap();
        resolver.hint_is_field(typ4, typ3, 0).unwrap();
        assert_eq!(resolver.fetch_final_ir_type(typ4), Some(IRType::Primitive(PrimitiveType::Bool)));
    }

    #[test]
    fn test_complicated() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ1, PrimitiveType::String).unwrap();
        resolver.hint_struct(typ3, 0, vec![typ1]).unwrap();
        resolver.hint_autoref(typ5, typ3).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ2]).unwrap();
        resolver.hint_is_ref(typ4, typ5).unwrap();
    }

    #[test]
    fn test_edge_case() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());
        let typ5 = resolver.new_type_label(FilePosition::unknown());
        resolver.hint_is(typ2, PrimitiveType::I32).unwrap();
        resolver.hint_struct(typ1, 0, vec![typ2]).unwrap();
        resolver.hint_struct(typ4, 0, vec![typ5]).unwrap();
        resolver.hint_struct(typ3, 0, vec![typ4]).unwrap();
        assert!(resolver.hint_equal(typ1, typ3).is_err());
    }

    #[test]
    fn test_edge_case2() {
        let mut resolver = TypeResolver::new(vec![vec![0]]);
        let typ1 = resolver.new_type_label(FilePosition::unknown());
        let typ2 = resolver.new_type_label(FilePosition::unknown());
        let typ3 = resolver.new_type_label(FilePosition::unknown());
        let typ4 = resolver.new_type_label(FilePosition::unknown());

        resolver.hint_struct(typ2, 0, vec![typ1]).unwrap();
        resolver.hint_is_field(typ3, typ2, 0).unwrap();
        resolver.hint_autoref(typ4, typ3).unwrap();
        resolver.hint_is(typ4, PrimitiveType::F32).unwrap();
        resolver.hint_equal(typ4, typ3).unwrap();

        assert_eq!(
            resolver.fetch_final_ir_type(typ2).unwrap(),
            IRType::Struct(0, vec![IRType::Primitive(PrimitiveType::F32)])
        );
    }
}
