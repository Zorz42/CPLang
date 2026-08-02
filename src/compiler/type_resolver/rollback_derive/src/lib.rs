use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Rollback)]
pub fn derive_rollback(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let mut generics = input.generics.clone();

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => panic!("Rollback can only be derived for structs with named fields"),
        },
        _ => panic!("Rollback can only be derived for structs"),
    };

    let field_names: Vec<_> = fields
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect();

    let field_types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    // Add bounds: every field must implement Rollback
    for ty in &field_types {
        generics
            .make_where_clause()
            .predicates
            .push(syn::parse_quote!(#ty: Rollback));
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics Rollback for #name #ty_generics #where_clause {
            type SaveState = (
                #(
                    <#field_types as Rollback>::SaveState,
                )*
            );

            fn save_state(&mut self) -> Self::SaveState {
                (
                    #(
                        self.#field_names.save_state(),
                    )*
                )
            }

            fn restore_state(&mut self, state: Self::SaveState) {
                let (
                    #(
                        #field_names,
                    )*
                ) = state;

                #(
                    self.#field_names.restore_state(#field_names);
                )*
            }
        }
    };

    expanded.into()
}