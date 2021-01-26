use std::mem::replace;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::Nothing,
    parse_macro_input, parse_quote,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn nested(args: TokenStream, input: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    let mut item_fn = parse_macro_input!(input as syn::ItemFn);
    let mut nested_fn = NestedFn::default();
    // visit item_fn in default way
    visit_mut::visit_item_fn_mut(&mut nested_fn, &mut item_fn);

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
    (quote! {
        #[allow(non_camel_case_types)]
        struct #ident (#(#struct_types),*);
        impl #ident {
            #vis #constness #asyncness #unsafety #abi #fn_token new() -> Self {
                Self(#(#struct_inits),*)
            }
            #(#attrs)* #vis #constness #asyncness #unsafety #abi #fn_token nest #generics (&mut self, #inputs #variadic) #output #block
        }
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
        Ok((
            match self.pat.clone() {
                syn::Pat::Type(pt) => Ok(*pt.ty),
                _ => Err(syn::Error::new(
                    self.span(),
                    "Call locals require explicit type",
                )),
            }?,
            match &mut self.init {
                Some((_, expr)) => Ok(replace(&mut **expr, parse_quote!(self.#index))),
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
        let index = syn::Index::from(index as usize);
        let func_path = match &mut *self.func {
            syn::Expr::Path(func_expr) => Ok(func_expr),
            func => Err(syn::Error::new(
                func.span(),
                "Nesting calls is only allow by path calls",
            )),
        }?;
        let type_path: syn::Type = syn::parse2(func_path.to_token_stream())?;
        let new_func_call: syn::Expr = parse_quote!(#func_path::new());

        func_path.path.segments.push(parse_quote!(nest));
        self.args.insert(0, parse_quote!(&mut self.#index));

        Ok((type_path, new_func_call))
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
