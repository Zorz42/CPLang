//! The macros behind `cplang::call_counter`.
//!
//! Both macros expand to nothing at all unless the crate they are used in is
//! built with `--features count_calls`.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    Error, Ident, ItemFn, LitInt, LitStr, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Keep in sync with `cplang::call_counter::MAX_TRACE_DEPTH`, which is the
/// number of frames a recorded call path has room for.
const MAX_TRACE_DEPTH: usize = 16;

/// Arguments shared by `#[count_calls]` and `count_call!`.
///
/// Both accept an optional call-trace depth and an optional display name, in
/// any order: bare (`3`, `"name"`) or named (`depth = 3`, `name = "name"`).
struct CountArgs {
    depth: Option<usize>,
    name: Option<LitStr>,
}

impl Parse for CountArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut args = Self { depth: None, name: None };

        while !input.is_empty() {
            if input.peek(LitInt) {
                args.set_depth(&input.parse::<LitInt>()?)?;
            } else if input.peek(LitStr) {
                args.set_name(input.parse::<LitStr>()?)?;
            } else {
                let key = input.parse::<Ident>()?;
                input.parse::<Token![=]>()?;

                if key == "depth" {
                    args.set_depth(&input.parse::<LitInt>()?)?;
                } else if key == "name" {
                    args.set_name(input.parse::<LitStr>()?)?;
                } else {
                    return Err(Error::new_spanned(key, "expected `depth` or `name`"));
                }
            }

            if input.is_empty() {
                break;
            }
            input.parse::<Token![,]>()?;
        }

        Ok(args)
    }
}

impl CountArgs {
    fn set_depth(&mut self, literal: &LitInt) -> syn::Result<()> {
        if self.depth.is_some() {
            return Err(Error::new_spanned(literal, "call trace depth given twice"));
        }

        let depth = literal.base10_parse::<usize>()?;
        if depth == 0 {
            return Err(Error::new_spanned(literal, "call trace depth must be at least 1"));
        }
        if depth > MAX_TRACE_DEPTH {
            return Err(Error::new_spanned(literal, format!("call trace depth must be at most {MAX_TRACE_DEPTH}")));
        }

        self.depth = Some(depth);
        Ok(())
    }

    fn set_name(&mut self, literal: LitStr) -> syn::Result<()> {
        if self.name.is_some() {
            return Err(Error::new_spanned(literal, "display name given twice"));
        }

        self.name = Some(literal);
        Ok(())
    }
}

/// Expands to the registration of one counter, yielding its id.
///
/// The counter is a function-local static, which keeps it one-per-site while
/// staying valid inside an `impl` block. Note that a static inside a generic
/// function is shared by every instantiation, so a generic function is counted
/// as a whole rather than per type argument.
fn counter_setup(name: &TokenStream2) -> TokenStream2 {
    quote! {
        static __CALL_COUNTER: ::std::sync::atomic::AtomicU64 = ::std::sync::atomic::AtomicU64::new(0);
        static __CALL_ID: ::std::sync::OnceLock<u32> = ::std::sync::OnceLock::new();

        __CALL_COUNTER.fetch_add(1, ::std::sync::atomic::Ordering::Relaxed);

        let __call_id = *__CALL_ID.get_or_init(|| ::cplang::call_counter::register(#name, &__CALL_COUNTER));
    }
}

/// Counts calls to a free function or an associated method.
///
/// `#[count_calls]` records how often the function ran.
/// `#[count_calls(depth = 3)]` additionally records where those calls came
/// from, keeping the two callers above every call; `#[count_calls(3)]` is a
/// shorter equivalent. The depth counts the function itself, so the default
/// depth of 1 is a plain total. Callers are read off the stack and need no
/// annotation of their own.
///
/// `#[count_calls(name = "Dsu::get_repr")]` overrides the reported name, which
/// is otherwise the module path plus the function name. That is worth doing
/// when two functions in one module share a name, as methods of different
/// types in the same module do.
#[proc_macro_attribute]
pub fn count_calls(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as CountArgs);
    let mut function = parse_macro_input!(item as ItemFn);

    if function.sig.constness.is_some() {
        return Error::new_spanned(&function.sig, "#[count_calls] cannot be used on const functions")
            .to_compile_error()
            .into();
    }

    let function_name = &function.sig.ident;
    let name = args
        .name
        .map_or_else(|| quote!(concat!(module_path!(), "::", stringify!(#function_name))), |name| quote!(#name));
    let depth = args.depth.unwrap_or(1);
    let setup = counter_setup(&name);
    // Below depth 2 there are no callers to look for, so the id is only needed
    // to get the counter registered.
    let record = if depth > 1 {
        // Expanded here rather than called through a helper on purpose: the
        // walk has to run in this function's own stack frame to see its caller.
        quote!(::cplang::call_counter::record_callers(__call_id, #depth);)
    } else {
        TokenStream2::new()
    };
    let original_body = &function.block;

    function.block = Box::new(syn::parse_quote!({
        // This entire block is removed before code generation unless the
        // application's `count_calls` feature is enabled.
        #[cfg(feature = "count_calls")]
        {
            #setup
            #record
        }

        #original_body
    }));

    quote!(#function).into()
}

/// Counts executions of an inline code location.
///
/// `count_call!()` identifies the location by module path, source file and
/// line, and `count_call!("label")` by a readable name. Like `#[count_calls]`,
/// it accepts a depth (`count_call!("label", 2)`) to also record which
/// functions the location ran inside of, starting with the one containing it.
#[proc_macro]
pub fn count_call(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input as CountArgs);

    let name = args
        .name
        .map_or_else(|| quote!(concat!(module_path!(), "::", file!(), ":", line!())), |name| quote!(#name));
    let depth = args.depth.unwrap_or(1);
    let setup = counter_setup(&name);

    // Below depth 2 there are no callers to attribute the run to, so the id is
    // only needed to get the counter registered.
    let record = if depth > 1 {
        quote!(::cplang::call_counter::record_site(__call_id, #depth);)
    } else {
        TokenStream2::new()
    };

    quote!({
        // Like `#[count_calls]`, this compiles away unless the dedicated
        // feature is enabled.
        #[cfg(feature = "count_calls")]
        {
            #setup
            #record
        }
    })
    .into()
}
