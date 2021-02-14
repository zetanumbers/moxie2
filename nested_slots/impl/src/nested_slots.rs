use crate::{local_slots::*, utils};
use derive_syn_parse::Parse;
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;

pub fn nested_slots(args: Args, mut input: utils::UniversalItemFn) -> syn::Result<Api> {
    return {
        if let Some(block) = &mut input.block {
            NestMacroExpander.visit_block_mut(block);
        }
        local_slots(args, input)
    };

    struct NestMacroExpander;

    impl VisitMut for NestMacroExpander {
        fn visit_item_mut(&mut self, _: &mut syn::Item) {
            // ignore
        }
        fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
            match expr {
                syn::Expr::Call(call) => {
                    if utils::consume_attribute(&mut call.attrs, &syn::parse_quote! {#[nest]}) {
                        *expr = syn::parse2(call.into_token_stream())
                            .and_then(expand_nest_macro)
                            .map_err(syn::Error::into_compile_error)
                            .map_or_else(syn::Expr::Verbatim, syn::Expr::Call)
                    }
                }
                syn::Expr::Macro(syn::ExprMacro {
                    attrs,
                    mac: syn::Macro { path, tokens, .. },
                }) if attrs.is_empty()
                    && match path.get_ident() {
                        Some(ident) => ident == "nest",
                        None => false,
                    } =>
                {
                    *expr = syn::parse2(std::mem::take(tokens))
                        .and_then(expand_nest_macro)
                        .map_err(syn::Error::into_compile_error)
                        .map_or_else(syn::Expr::Verbatim, syn::Expr::Call)
                }

                _ => (),
            }
            syn::visit_mut::visit_expr_mut(self, expr);

            fn expand_nest_macro(mut call: Call) -> syn::Result<syn::ExprCall> {
                let init_path = syn::ExprPath {
                    attrs: Vec::new(),
                    path: InterfaceTy::InitFn(Default::default())
                        .mangle_path(call.func.path.clone())?,
                    qself: call.func.qself.clone(),
                };
                let type_path = syn::ExprPath {
                    attrs: Vec::new(),
                    path: InterfaceTy::Type(Default::default())
                        .mangle_path(call.func.path.clone())?,
                    qself: call.func.qself.clone(),
                };

                call.func.path =
                    InterfaceTy::EnterFn(Default::default()).mangle_path(call.func.path)?;
                call.args.push(syn::parse2(
                    quote! { &mut local_slot!(#init_path(init_context) as #type_path) },
                )?);

                syn::parse2(call.into_token_stream())
            }
        }
    }

    #[derive(Debug, Parse)]
    struct Call {
        #[call(syn::Attribute::parse_outer)]
        attrs: Vec<syn::Attribute>,
        func: syn::ExprPath,
        #[paren]
        paren_token: syn::token::Paren,
        #[inside(paren_token)]
        #[parse_terminated(syn::Expr::parse)]
        args: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]>,
    }

    impl quote::ToTokens for Call {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let Self {
                attrs,
                func,
                paren_token,
                args,
            } = self;
            attrs.iter().for_each(|attr| attr.to_tokens(tokens));
            func.to_tokens(tokens);
            paren_token.surround(tokens, |tokens| args.to_tokens(tokens));
        }
    }
}

pub type Args = crate::local_slots::Args;
