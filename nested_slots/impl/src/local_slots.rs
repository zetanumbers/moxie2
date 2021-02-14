use crate::utils;
use darling::FromMeta;
use derive_syn_parse::Parse;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use syn::spanned::Spanned;

pub fn local_slots(
    Args {
        outside_module,
        bounds,
    }: Args,
    mut input: utils::UniversalItemFn,
) -> syn::Result<Api> {
    use syn::visit_mut::VisitMut;
    return {
        let namespace = if outside_module {
            quote! {Self}
        } else {
            quote! {self}
        };

        let (ty_def, init_fn_block): (TokenStream2, TokenStream2) =
            if let Some(block) = &mut input.block {
                let mut expander = LocalSlotMacroExpander(Vec::new());
                expander.visit_block_mut(block);
                let (tys, exprs) = expander.iter_pair();
                (
                    quote! { = (#(#tys,)*); },
                    quote! {
                        {
                            (#(#exprs,)*)
                        }
                    },
                )
            } else {
                (quote! {;}, quote! {;})
            };

        let enter_ctx = InterfaceTy::EnterCtx(Default::default()).into_mangled_ident();
        let init_ctx_ty = InterfaceTy::InitCtxTy(Default::default()).into_mangled_ident();

        let init_ctx_ty_bounds = if bounds.is_empty() {
            quote! {}
        } else {
            quote! {: #bounds}
        };

        let type_name = InterfaceTy::Type(Default::default()).mangle_ident(&input.sig.ident);
        let init_fn_name = InterfaceTy::InitFn(Default::default()).mangle_ident(&input.sig.ident);
        input.sig.ident = InterfaceTy::EnterFn(Default::default()).mangle_ident(&input.sig.ident);

        input.sig.inputs.push({
            let ty: syn::Type = syn::parse2(quote! { #namespace :: #type_name })?;
            let out = syn::parse2(quote! { #enter_ctx : &mut #ty })?;
            out
        });
        let vis = &input.vis;

        Ok(Api {
            ty: syn::parse2(
                quote! { #[allow(non_camel_case_types)] #vis type #type_name #ty_def },
            )?,
            init_fn: syn::parse2(quote! {
                #vis fn #init_fn_name <#init_ctx_ty #init_ctx_ty_bounds>
                (init_context: &mut #init_ctx_ty) -> #namespace :: #type_name #init_fn_block
            })?,
            enter_fn: input,
        })
    };

    struct LocalSlotMacroExpander(Vec<(syn::Type, syn::Expr)>);

    impl LocalSlotMacroExpander {
        fn iter_pair<'a>(
            &'a self,
        ) -> (
            impl Iterator<Item = &syn::Type> + 'a,
            impl Iterator<Item = &syn::Expr> + 'a,
        ) {
            (self.0.iter().map(|p| &p.0), self.0.iter().map(|p| &p.1))
        }
    }

    impl syn::visit_mut::VisitMut for LocalSlotMacroExpander {
        fn visit_item_mut(&mut self, _: &mut syn::Item) {
            // ignore
        }
        fn visit_local_mut(&mut self, local: &mut syn::Local) {
            if utils::consume_attribute(&mut local.attrs, &syn::parse_quote! {#[local_slot]}) {
                local.init.as_mut().unwrap().1 = Box::new(
                    syn::parse2(local.to_token_stream())
                        .and_then(expand_local_slot_macro_local)
                        .map_err(syn::Error::into_compile_error)
                        .map_or_else(syn::Expr::Verbatim, syn::Expr::Macro),
                );
            }
            syn::visit_mut::visit_local_mut(self, local);

            #[derive(Parse)]
            #[allow(dead_code)]
            struct LocalForSlot {
                #[call(syn::Attribute::parse_outer)]
                attrs: Vec<syn::Attribute>,
                let_token: syn::Token![let],
                pat: syn::Pat,
                colon_token: syn::Token![:],
                ty: syn::Type,
                eq_token: syn::Token![=],
                expr: syn::Expr,
                semi_token: syn::Token![;],
            }

            fn expand_local_slot_macro_local(local: LocalForSlot) -> syn::Result<syn::ExprMacro> {
                let ty = local.ty.clone();
                let expr = local.expr.clone();
                syn::parse2(quote! { local_slot!(#expr as #ty) })
            }
        }
        fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
            // replace expr with empty `TokenStream` then process its previous value and assign that back to `*expr`
            *expr = match std::mem::replace(expr, syn::Expr::Verbatim(Default::default())) {
                syn::Expr::Paren(mut expr_paren) => match *expr_paren.expr {
                    syn::Expr::Cast(cast)
                        if utils::consume_attribute(
                            &mut expr_paren.attrs,
                            &syn::parse_quote! {#[local_slot]},
                        ) =>
                    {
                        self.expand_local_slot_macro(cast)
                            .map_err(syn::Error::into_compile_error)
                            .map_or_else(syn::Expr::Verbatim, syn::Expr::Field)
                    }
                    expr => {
                        *expr_paren.expr = expr;
                        syn::Expr::Paren(expr_paren)
                    }
                },
                syn::Expr::Macro(syn::ExprMacro {
                    attrs,
                    mac: syn::Macro { path, tokens, .. },
                }) if attrs.is_empty()
                    && match path.get_ident() {
                        Some(ident) => ident == "local_slot",
                        None => false,
                    } =>
                {
                    syn::parse2::<syn::ExprCast>(tokens)
                        .and_then(|cast| self.expand_local_slot_macro(cast))
                        .map_err(syn::Error::into_compile_error)
                        .map_or_else(syn::Expr::Verbatim, syn::Expr::Field)
                }
                expr => expr,
            };
            syn::visit_mut::visit_expr_mut(self, expr);
        }
    }

    impl LocalSlotMacroExpander {
        fn expand_local_slot_macro(
            &mut self,
            syn::ExprCast { ty, expr, .. }: syn::ExprCast,
        ) -> syn::Result<syn::ExprField> {
            let ctx = InterfaceTy::EnterCtx(Default::default()).into_mangled_ident();
            let i = syn::Index::from(self.0.len());
            self.0.push((*ty, *expr));
            syn::parse2(quote! { #ctx . #i })
        }
    }
}

pub fn local_slots_interface(
    InterfaceReq { interface, path }: InterfaceReq,
) -> syn::Result<syn::Path> {
    match path {
        Some(path) => interface.mangle_path(path),
        None => syn::parse2(interface.into_mangled_ident().into_token_stream()),
    }
}

#[derive(Parse)]
pub struct InterfaceReq {
    pub interface: InterfaceTy,
    #[peek(syn::Ident)]
    pub path: Option<syn::Path>,
}

#[derive(Debug, Clone, Copy, Parse)]
pub enum InterfaceTy {
    #[peek(syn::Token![type], name = "Type")]
    Type(syn::Token![type]),
    #[peek(kw::init, name = "InitFn")]
    InitFn(kw::init),
    #[peek(kw::enter, name = "EnterFn")]
    EnterFn(kw::enter),
    #[peek(kw::context, name = "EnterCtx")]
    EnterCtx(kw::context),
    #[peek(kw::init_context_type, name = "InitCtxTy")]
    InitCtxTy(kw::init_context_type),
}

impl quote::ToTokens for InterfaceTy {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            InterfaceTy::Type(tk) => tk.to_tokens(tokens),
            InterfaceTy::InitFn(tk) => tk.to_tokens(tokens),
            InterfaceTy::EnterFn(tk) => tk.to_tokens(tokens),
            InterfaceTy::EnterCtx(tk) => tk.to_tokens(tokens),
            InterfaceTy::InitCtxTy(tk) => tk.to_tokens(tokens),
        }
    }
}

impl quote::IdentFragment for InterfaceTy {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InterfaceTy::Type(_) => "type",
            InterfaceTy::InitFn(_) => "init",
            InterfaceTy::EnterFn(_) => "enter",
            InterfaceTy::EnterCtx(_) => "context",
            InterfaceTy::InitCtxTy(_) => "init_context_type",
        }
        .fmt(f)
    }

    fn span(&self) -> Option<Span> {
        Some(match self {
            InterfaceTy::Type(tk) => tk.span(),
            InterfaceTy::InitFn(tk) => tk.span(),
            InterfaceTy::EnterFn(tk) => tk.span(),
            InterfaceTy::EnterCtx(tk) => tk.span(),
            InterfaceTy::InitCtxTy(tk) => tk.span(),
        })
    }
}

impl InterfaceTy {
    pub fn into_mangled_ident(self) -> syn::Ident {
        const SUFFIX_UUID: &str = "e67dd0c1_f2a8_4161_aa1b_18cdaec4e496";
        format_ident!("{}_{}_{}", self, env!("CARGO_PKG_NAME"), SUFFIX_UUID)
    }

    fn mangle_ident(self, ident: &syn::Ident) -> syn::Ident {
        format_ident!("{}_{}", ident, self.into_mangled_ident())
    }

    pub fn mangle_path(self, mut path: syn::Path) -> syn::Result<syn::Path> {
        let span = path.span();
        let last_ident = &mut path
            .segments
            .last_mut()
            .ok_or_else(|| syn::Error::new(span, "Expected nonempty path"))?
            .ident;
        *last_ident = self.mangle_ident(last_ident);
        Ok(path)
    }
}

mod kw {
    syn::custom_keyword!(init);
    syn::custom_keyword!(enter);
    syn::custom_keyword!(context);
    syn::custom_keyword!(init_context_type);
}

#[derive(FromMeta, Default)]
pub struct Args {
    #[darling(default)]
    pub outside_module: bool,
    #[darling(default)]
    pub bounds: syn::punctuated::Punctuated<syn::TraitBound, syn::Token![+]>,
}

#[derive(Debug, Parse, PartialEq, Eq)]
pub struct Api {
    pub ty: utils::UniversalItemType,
    pub init_fn: utils::UniversalItemFn,
    pub enter_fn: utils::UniversalItemFn,
}

impl quote::ToTokens for Api {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            ty,
            init_fn,
            enter_fn,
        } = self;
        ty.to_tokens(tokens);
        init_fn.to_tokens(tokens);
        enter_fn.to_tokens(tokens);
    }
}
