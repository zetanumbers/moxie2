use darling::FromMeta;
use nested_slots_impl as impl_;
use proc_macro::TokenStream;
use quote::ToTokens;

#[proc_macro_attribute]
pub fn local_slots(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args: syn::AttributeArgs = syn::parse_macro_input!(args);

    let args = match impl_::Args::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let input: impl_::UniversalItemFn = syn::parse_macro_input!(input);

    match impl_::local_slots(args, input) {
        Ok(api) => api.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

#[proc_macro]
pub fn local_slots_interface(input: TokenStream) -> TokenStream {
    let input: impl_::InterfaceReq = syn::parse_macro_input!(input);

    match impl_::local_slots_interface(input) {
        Ok(ident) => ident.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn nested_slots(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args: syn::AttributeArgs = syn::parse_macro_input!(args);

    let args = match impl_::Args::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let input: impl_::UniversalItemFn = syn::parse_macro_input!(input);

    match impl_::nested_slots(args, input) {
        Ok(api) => api.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}
