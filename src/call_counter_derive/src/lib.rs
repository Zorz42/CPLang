use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Error, ItemFn, LitStr};

/// Counts calls to a free function or an associated method.
///
/// The generated counter is local to the annotated function, which makes the
/// attribute valid inside `impl` blocks as well as at module scope.
#[proc_macro_attribute]
pub fn count_calls(_args: TokenStream, item: TokenStream) -> TokenStream {
    let mut function = parse_macro_input!(item as ItemFn);

    if function.sig.constness.is_some() {
        return Error::new_spanned(
            &function.sig,
            "#[count_calls] cannot be used on const functions",
        )
        .to_compile_error()
        .into();
    }

    let function_name = &function.sig.ident;
    let original_body = &function.block;

    function.block = Box::new(syn::parse_quote!({
        // This entire block is removed before code generation unless the
        // application's `count_calls` feature is enabled.
        #[cfg(feature = "count_calls")]
        {
            // Function-local statics are still one-per-function, including
            // for methods in an `impl`, while avoiding sibling items.
            static __CALL_COUNTER: ::std::sync::atomic::AtomicU64 =
                ::std::sync::atomic::AtomicU64::new(0);
            static __CALL_COUNTER_REGISTER: ::std::sync::Once = ::std::sync::Once::new();

            __CALL_COUNTER_REGISTER.call_once(|| {
                ::cplang::call_counter::register(
                    concat!(module_path!(), "::", stringify!(#function_name)),
                    &__CALL_COUNTER,
                );
            });

            __CALL_COUNTER.fetch_add(1, ::std::sync::atomic::Ordering::Relaxed);
        }

        #original_body
    }));

    quote!(#function).into()
}

/// Counts executions of an inline code location.
///
/// Use `count_call!()` to identify the location by source file and line, or
/// `count_call!("label")` to supply a readable name.
#[proc_macro]
pub fn count_call(input: TokenStream) -> TokenStream {
    let name = if input.is_empty() {
        quote!(concat!(module_path!(), "::", file!(), ":", line!()))
    } else {
        let label = parse_macro_input!(input as LitStr);
        quote!(#label)
    };

    quote!({
        // Like `#[count_calls]`, this compiles away unless the dedicated
        // feature is enabled.
        #[cfg(feature = "count_calls")]
        {
            static __CALL_COUNTER: ::std::sync::atomic::AtomicU64 =
                ::std::sync::atomic::AtomicU64::new(0);
            static __CALL_COUNTER_REGISTER: ::std::sync::Once = ::std::sync::Once::new();

            __CALL_COUNTER_REGISTER.call_once(|| {
                ::cplang::call_counter::register(#name, &__CALL_COUNTER);
            });

            __CALL_COUNTER.fetch_add(1, ::std::sync::atomic::Ordering::Relaxed);
        }
    })
    .into()
}
