
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
