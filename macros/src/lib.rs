use std::{convert::Infallible, mem::replace};

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse,
    parse_macro_input, parse_quote,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
};

const SUFFIX_UUID: &str = "e67dd0c1_f2a8_4161_aa1b_18cdaec4e496";

#[derive(Debug, Clone, Copy)]
enum Suffix {
    Type,
    New,
    Nest,
}

impl quote::IdentFragment for Suffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Suffix::Type => f.write_str("type"),
            Suffix::New => f.write_str("new"),
            Suffix::Nest => f.write_str("nest"),
        }
    }
}

impl Parse for Suffix {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        match ident.to_string().as_str() {
            "type" => Ok(Suffix::Type),
            "new" => Ok(Suffix::New),
            "nest" => Ok(Suffix::Nest),
            _ => Err(input.error("expected `type`, `new` or `nest`")),
        }
    }
}

struct NestedInterfaceRequest(syn::Path, Suffix);

impl Parse for NestedInterfaceRequest {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(input.parse()?, input.parse()?))
    }
}

impl NestedInterfaceRequest {
    fn into_nested_interface(self) -> syn::Result<syn::Path> {
        self.0.produce_name(self.1)
    }
}

fn make_ident_var_ctx() -> syn::Ident {
    format_ident!("{}_{}_ctx", env!("CARGO_PKG_NAME"), SUFFIX_UUID)
}

trait MangleFactory: Sized {
    type Error;

    fn produce_name(&self, suffix: Suffix) -> Result<Self, Self::Error>;
}

impl MangleFactory for syn::Ident {
    type Error = Infallible;

    fn produce_name(&self, suffix: Suffix) -> Result<Self, Self::Error> {
        Ok(format_ident!(
            "{}_{}_{}_{}",
            self,
            env!("CARGO_PKG_NAME"),
            SUFFIX_UUID,
            suffix
        ))
    }
}

fn get_path_last_ident(path: &mut syn::Path) -> syn::Result<&mut syn::Ident> {
    let span = path.span();
    match path.segments.last_mut() {
        Some(syn::PathSegment { ident, .. }) => Ok(ident),
        None => Err(syn::Error::new(span, "Expected a path segment")),
    }
}
impl MangleFactory for syn::Path {
    type Error = syn::Error;

    fn produce_name(&self, suffix: Suffix) -> Result<Self, Self::Error> {
        let mut path = self.clone();
        let ident = get_path_last_ident(&mut path)?;
        *ident = into_ok(ident.produce_name(suffix));
        Ok(path)
    }
}
fn into_ok<T>(result: Result<T, Infallible>) -> T {
    match result {
        Ok(item) => item,
        Err(_) => unreachable!(),
    }
}

#[proc_macro]
pub fn nested_interface(input: TokenStream) -> TokenStream {
    match parse_macro_input!(input as NestedInterfaceRequest).into_nested_interface() {
        Ok(path) => path.into_token_stream(),
        Err(error) => error.into_compile_error(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn nested(args: TokenStream, input: TokenStream) -> TokenStream {
    let environment =
        parse_macro_input!(args as Option<syn::Ident>).unwrap_or(format_ident!("self"));
    let mut item_fn = parse_macro_input!(input as syn::ItemFn);
    let mut nested_fn = NestedFn::default();
    nested_fn.visit_block_mut(&mut item_fn.block);

    // TODO add generics support
    let syn::ItemFn {
        attrs,
        vis,
        sig:
            syn::Signature {
                constness,
                asyncness,
                unsafety,
                abi,
                fn_token,
                ident,
                generics,
                paren_token: _,
                mut inputs,
                variadic,
                output,
            },
        block,
    } = item_fn;
    if !inputs.empty_or_trailing() {
        inputs.push_punct(parse_quote!(,))
    }

    let struct_types = nested_fn.collection.iter().map(|(ty, _)| ty);
    let struct_inits = nested_fn.collection.iter().map(|(_, expr)| expr);

    let type_ident = into_ok(ident.produce_name(Suffix::Type));
    let new_fn_ident = into_ok(ident.produce_name(Suffix::New));
    let nest_fn_ident = into_ok(ident.produce_name(Suffix::Nest));
    drop(ident);

    let ctx_var_ident = make_ident_var_ctx();
    (quote! {
        #[allow(non_camel_case_types)]
        #vis type #type_ident = (#(#struct_types,)*);
        #vis #constness #asyncness #unsafety #abi #fn_token #new_fn_ident () -> #environment :: #type_ident {
            (#(#struct_inits,)*)
        }
        #(#attrs)* #vis #constness #asyncness #unsafety #abi #fn_token #nest_fn_ident #generics (#inputs #ctx_var_ident : &mut #environment :: #type_ident , #variadic) #output #block
    }).into()
}

#[derive(Default, Debug)]
struct NestedFn {
    collection: Vec<(syn::Type, syn::Expr)>,
    error_occured: bool,
}

impl NestedFn {
    fn transform_add<T>(&mut self, element: &mut T)
    where
        T: TryExpand + EmbededError,
    {
        if !self.error_occured {
            match element.try_expand(self.collection.len()) {
                Ok(p) => self.collection.push(p),
                Err(error) => {
                    *element = T::embeded_error(error);
                    self.error_occured |= true;
                }
            }
        }
    }
}

impl VisitMut for NestedFn {
    fn visit_local_mut(&mut self, local: &mut syn::Local) {
        if resolve_attribute(&mut local.attrs, parse_quote!(#[call_local])) {
            self.transform_add(local);
        }

        visit_mut::visit_local_mut(self, local)
    }
    fn visit_expr_call_mut(&mut self, call: &mut syn::ExprCall) {
        if resolve_attribute(&mut call.attrs, parse_quote!(#[nest])) {
            self.transform_add(call);
        }

        visit_mut::visit_expr_call_mut(self, call)
    }
    fn visit_item_fn_mut(&mut self, _: &mut syn::ItemFn) {
        // prevent visiting nested functions
    }
}

fn resolve_attribute(attrs: &mut Vec<syn::Attribute>, resolve_attr: syn::Attribute) -> bool {
    if let Some(i) = attrs.iter().position(|attr| *attr == resolve_attr) {
        attrs.remove(i);
        true
    } else {
        false
    }
}

trait TryExpand {
    fn try_expand(&mut self, index: usize) -> syn::Result<(syn::Type, syn::Expr)>;
}

impl TryExpand for syn::Local {
    fn try_expand(&mut self, index: usize) -> syn::Result<(syn::Type, syn::Expr)> {
        let index = syn::Index::from(index);
        let ctx_ident = make_ident_var_ctx();
        Ok((
            match self.pat.clone() {
                syn::Pat::Type(pt) => Ok(*pt.ty),
                _ => Err(syn::Error::new(
                    self.span(),
                    "Call locals require explicit type",
                )),
            }?,
            match &mut self.init {
                Some((_, expr)) => Ok(replace(&mut **expr, parse_quote!(#ctx_ident . #index))),
                None => Err(syn::Error::new(
                    self.span(),
                    "Call locals require initialization",
                )),
            }?,
        ))
    }
}

impl TryExpand for syn::ExprCall {
    fn try_expand(&mut self, index: usize) -> syn::Result<(syn::Type, syn::Expr)> {
        let expr_path = match &mut *self.func {
            syn::Expr::Path(expr_path) => Ok(expr_path),
            func => Err(syn::Error::new(
                func.span(),
                "Nesting calls is only allow by path calls",
            )),
        }?;
        let new_path = expr_path.path.produce_name(Suffix::Nest)?;
        let syn::ExprPath { attrs, qself, path } = syn::ExprPath {
            path: replace(&mut expr_path.path, new_path),
            ..expr_path.clone()
        };
        drop(expr_path);

        let type_path = syn::Type::Path(syn::TypePath {
            qself: qself.clone(),
            path: path.produce_name(Suffix::Type)?,
        });
        let new_fn_path = syn::Expr::Path(syn::ExprPath {
            attrs: attrs.clone(),
            qself: qself.clone(),
            path: path.produce_name(Suffix::New)?,
        });
        let nest_fn_path = syn::Expr::Path(syn::ExprPath {
            attrs,
            qself,
            path: path.produce_name(Suffix::Nest)?,
        });

        let new_fn_call = syn::Expr::Call(syn::ExprCall {
            attrs: self.attrs.clone(),
            func: Box::new(new_fn_path),
            paren_token: Default::default(),
            args: Default::default(),
        });

        let index = syn::Index::from(index);
        let ctx_ident = make_ident_var_ctx();
        self.args.push(parse_quote!(&mut #ctx_ident . #index));
        self.func = Box::new(nest_fn_path);

        Ok((type_path, new_fn_call))
    }
}

trait EmbededError {
    fn embeded_error(error: syn::Error) -> Self;
}

impl EmbededError for syn::ExprCall {
    fn embeded_error(error: syn::Error) -> Self {
        syn::ExprCall {
            attrs: Vec::new(),
            func: Box::new(syn::Expr::Verbatim(error.into_compile_error())),
            paren_token: Default::default(),
            args: Default::default(),
        }
    }
}

impl EmbededError for syn::Local {
    fn embeded_error(error: syn::Error) -> Self {
        syn::Local {
            attrs: Vec::new(),
            let_token: Default::default(),
            pat: syn::Pat::Verbatim(error.into_compile_error()),
            init: None,
            semi_token: Default::default(),
        }
    }
}
