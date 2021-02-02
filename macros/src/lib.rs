use darling::FromMeta;
use proc_macro::TokenStream;
use quote::ToTokens;

mod local_slots {
    use crate::utils;
    use darling::FromMeta;
    use derive_syn_parse::Parse;
    use proc_macro2::{Span, TokenStream as TokenStream2};
    use quote::{format_ident, quote, ToTokens, TokenStreamExt};
    use syn::spanned::Spanned;

    pub fn local_slots_impl(
        Args { namespace }: Args,
        mut input: utils::UniversalItemFn,
    ) -> syn::Result<Api> {
        use syn::visit_mut::VisitMut;
        return {
            let ctx = InterfaceTy::EnterCtx(Default::default()).into_mangled_ident();

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

            let type_name = InterfaceTy::Type(Default::default()).mangle_ident(&input.sig.ident);
            let init_fn_name =
                InterfaceTy::InitFn(Default::default()).mangle_ident(&input.sig.ident);
            input.sig.ident =
                InterfaceTy::EnterFn(Default::default()).mangle_ident(&input.sig.ident);

            input.sig.inputs.push({
                let ty: syn::Type = syn::parse2(quote! { #namespace :: #type_name })?;
                let out = syn::parse2(quote! { #ctx : &mut #ty })?;
                out
            });
            let vis = &input.vis;

            Ok(Api {
                ty: syn::parse2(
                    quote! { #[allow(non_camel_case_types)] #vis type #type_name #ty_def },
                )?,
                init_fn: syn::parse2(
                    quote! { #vis fn #init_fn_name () -> #namespace :: #type_name #init_fn_block },
                )?,
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

                fn expand_local_slot_macro_local(
                    local: LocalForSlot,
                ) -> syn::Result<syn::ExprMacro> {
                    let ty = local.ty.clone();
                    let expr = local.expr.clone();
                    syn::parse2(quote! { local_slot!(#expr as #ty) })
                }
            }
            fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
                // replace expr with empty `TokenStream` then process its previous value and assign that back to `*expr`
                *expr = match std::mem::replace(expr, syn::Expr::Verbatim(Default::default())) {
                    syn::Expr::Paren(mut expr_paren) => {
                        *expr_paren.expr = match *expr_paren.expr {
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
                            expr => expr,
                        };
                        syn::Expr::Paren(expr_paren)
                    }
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

    pub fn local_slots_interface_impl(
        InterfaceReq { interface, path }: InterfaceReq,
    ) -> syn::Result<syn::Path> {
        match path {
            Some(path) => interface.mangle_path(path),
            None => {
                let ident = interface.into_mangled_ident();
                Ok(syn::parse_quote!(#ident))
            }
        }
    }

    #[derive(Parse)]
    pub struct InterfaceReq {
        interface: InterfaceTy,
        #[peek(syn::Ident)]
        path: Option<syn::Path>,
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
    }

    impl quote::ToTokens for InterfaceTy {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            match self {
                InterfaceTy::Type(tk) => tk.to_tokens(tokens),
                InterfaceTy::InitFn(tk) => tk.to_tokens(tokens),
                InterfaceTy::EnterFn(tk) => tk.to_tokens(tokens),
                InterfaceTy::EnterCtx(tk) => tk.to_tokens(tokens),
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
            }
            .fmt(f)
        }

        fn span(&self) -> Option<Span> {
            Some(match self {
                InterfaceTy::Type(tk) => tk.span(),
                InterfaceTy::InitFn(tk) => tk.span(),
                InterfaceTy::EnterFn(tk) => tk.span(),
                InterfaceTy::EnterCtx(tk) => tk.span(),
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
    }

    #[derive(FromMeta, Default)]
    pub struct Args {
        #[darling(default)]
        pub namespace: Namespace,
    }

    #[derive(Debug, Parse, PartialEq, Eq)]
    pub struct Api {
        ty: utils::UniversalItemType,
        init_fn: utils::UniversalItemFn,
        enter_fn: utils::UniversalItemFn,
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

    #[derive(FromMeta)]
    pub enum Namespace {
        #[darling(rename = "Self")]
        SelfType,
        #[darling(rename = "self")]
        SelfModule,
    }

    impl Default for Namespace {
        fn default() -> Self {
            Namespace::SelfModule
        }
    }

    impl quote::ToTokens for Namespace {
        fn to_tokens(&self, tokens: &mut TokenStream2) {
            tokens.append(proc_macro2::Ident::new(
                match self {
                    Namespace::SelfType => "Self",
                    Namespace::SelfModule => "self",
                },
                Span::call_site(),
            ))
        }
    }
}

mod nested_slots {
    use crate::{local_slots::*, utils};
    use derive_syn_parse::Parse;
    use quote::{quote, ToTokens};
    use syn::visit_mut::VisitMut;

    pub fn nested_slots_impl(args: Args, mut input: utils::UniversalItemFn) -> syn::Result<Api> {
        return {
            if let Some(block) = &mut input.block {
                NestMacroExpander.visit_block_mut(block);
            }
            local_slots_impl(args, input)
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
                        quote! { &mut local_slot!(#init_path() as #type_path) },
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
}

#[proc_macro_attribute]
pub fn local_slots(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args: syn::AttributeArgs = syn::parse_macro_input!(args);

    let args = match local_slots::Args::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let input: utils::UniversalItemFn = syn::parse_macro_input!(input);

    match local_slots::local_slots_impl(args, input) {
        Ok(api) => api.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

#[proc_macro]
pub fn local_slots_interface(input: TokenStream) -> TokenStream {
    let input: local_slots::InterfaceReq = syn::parse_macro_input!(input);

    match local_slots::local_slots_interface_impl(input) {
        Ok(ident) => ident.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn nested_slots(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args: syn::AttributeArgs = syn::parse_macro_input!(args);

    let args = match nested_slots::Args::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let input: utils::UniversalItemFn = syn::parse_macro_input!(input);

    match nested_slots::nested_slots_impl(args, input) {
        Ok(api) => api.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

mod utils {
    use derive_syn_parse::Parse;
    use syn::punctuated::Punctuated;

    #[derive(Debug, Parse, PartialEq, Eq)]
    pub struct UniversalItemType {
        #[call(syn::Attribute::parse_outer)]
        pub attrs: Vec<syn::Attribute>,
        pub vis: syn::Visibility,
        pub type_token: syn::Token![type],
        pub ident: syn::Ident,
        pub generics: syn::Generics,
        #[peek(syn::Token![:])]
        pub bounds: Option<ColonBounds>,
        #[peek(syn::Token![=])]
        pub ty: Option<EqType>,
        pub semi_token: syn::Token![;],
    }

    impl quote::ToTokens for UniversalItemType {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let Self {
                attrs,
                vis,
                type_token,
                ident,
                generics,
                bounds,
                ty,
                semi_token,
            } = self;
            attrs.iter().for_each(|attr| attr.to_tokens(tokens));
            vis.to_tokens(tokens);
            type_token.to_tokens(tokens);
            ident.to_tokens(tokens);
            generics.to_tokens(tokens);
            bounds.to_tokens(tokens);
            ty.to_tokens(tokens);
            semi_token.to_tokens(tokens);
        }
    }

    #[derive(Debug, Parse, PartialEq, Eq)]
    pub struct EqType {
        pub eq_token: syn::Token![=],
        pub ty: syn::Type,
    }

    impl quote::ToTokens for EqType {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let Self { eq_token, ty } = self;
            eq_token.to_tokens(tokens);
            ty.to_tokens(tokens);
        }
    }

    #[derive(Debug, Parse, PartialEq, Eq)]
    pub struct ColonBounds {
        pub colon_token: syn::Token![:],
        #[call(Punctuated::parse_separated_nonempty)]
        pub bounds: Punctuated<syn::TypeParamBound, syn::Token![+]>,
    }

    impl quote::ToTokens for ColonBounds {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let Self {
                colon_token,
                bounds,
            } = self;
            colon_token.to_tokens(tokens);
            bounds.to_tokens(tokens);
        }
    }

    #[derive(Debug, Parse, PartialEq, Eq)]
    pub struct UniversalItemFn {
        #[call(syn::Attribute::parse_outer)]
        pub attrs: Vec<syn::Attribute>,
        pub vis: syn::Visibility,
        pub defaultness: Option<syn::Token![default]>,
        pub sig: syn::Signature,
        #[peek(syn::token::Brace)]
        pub block: Option<syn::Block>,
        #[peek(syn::Token![;])]
        pub semi_token: Option<syn::Token![;]>,
    }

    impl quote::ToTokens for UniversalItemFn {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let Self {
                attrs,
                vis,
                defaultness,
                sig,
                block,
                semi_token,
            } = self;

            attrs.iter().for_each(|attr| attr.to_tokens(tokens));
            vis.to_tokens(tokens);
            defaultness.to_tokens(tokens);
            sig.to_tokens(tokens);
            block.to_tokens(tokens);
            semi_token.to_tokens(tokens);
        }
    }

    /// Returns true on consumption of one attribute from `attrs` if it equals to `attr_to_consume`
    pub fn consume_attribute(
        attrs: &mut Vec<syn::Attribute>,
        attr_to_consume: &syn::Attribute,
    ) -> bool {
        if let Some(i) = attrs.iter().position(|attr| attr == attr_to_consume) {
            attrs.remove(i);
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{local_slots::local_slots_interface_impl, nested_slots::nested_slots_impl};

    use super::*;
    use local_slots::Api;

    macro_rules! compare_foos {
        ($generate_from:literal, $expected:literal) => {
            let generated: Api = nested_slots_impl(
                local_slots::Args::default(),
                syn::parse_str($generate_from).unwrap(),
            )
            .unwrap();
            let expected: Api = syn::parse_str(
                &format!($expected,
                    foo_type = get_interface!(type foo).get_ident().unwrap(),
                    foo_init = get_interface!(init foo).get_ident().unwrap(),
                    foo_enter = get_interface!(enter foo).get_ident().unwrap(),
                    context = get_interface!(context).get_ident().unwrap(),
                )
            ).unwrap();
            assert_eq!(generated, expected);
        };
    }

    macro_rules! get_interface {
        ($($tt:tt)*) => {
            local_slots_interface_impl(syn::parse_quote!($($tt)*)).unwrap()
        };
    }

    #[test]
    fn no_body() {
        compare_foos!(
            "
                fn foo();
            ",
            "
                #[allow(non_camel_case_types)]
                type {foo_type};
                fn {foo_init}() -> self:: {foo_type};
                fn {foo_enter}({context}: &mut self:: {foo_type});
            "
        );
    }

    #[test]
    fn nested_slots() {
        compare_foos!(
            "
                fn foo() {
                    #[local_slot] let ref mut x: u8 = 3;
                    &mut #[local_slot] (2 as u8);
                    &mut local_slot!(1 as u8);

                    nest!(foo());
                    #[nest] foo();
                }
            ",
            "
                #[allow(non_camel_case_types)]
                type {foo_type} = (u8, u8, u8, {foo_type}, {foo_type},);
                fn {foo_init}() -> self:: {foo_type} {{ (3, 2, 1, {foo_init}(), {foo_init}(),) }}
                fn {foo_enter}({context}: &mut self:: {foo_type}) {{
                    let ref mut x: u8 = {context}.0;
                    &mut ({context}.1);
                    &mut {context}.2;

                    {foo_enter}(&mut {context}.3);
                    {foo_enter}(&mut {context}.4);
                }}
            "
        );
    }
}
