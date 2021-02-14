use nested_slots_impl as impl_;

macro_rules! compare_foos {
    ($args:expr, $generate_from:literal, $expected:literal) => {
        let generated: impl_::Api = impl_::nested_slots(
            $args,
            syn::parse_str($generate_from).expect("Invalid first argument"),
        )
        .expect("Unable to generate output");
        let expected: impl_::Api = syn::parse_str(&format!(
            $expected,
            type_suffix = get_interface!(type).get_ident().unwrap(),
            init_suffix = get_interface!(init).get_ident().unwrap(),
            enter_suffix = get_interface!(enter).get_ident().unwrap(),
            context = get_interface!(context).get_ident().unwrap(),
            init_context_type = get_interface!(init_context_type).get_ident().unwrap(),
        ))
        .expect("Invalid second argument");
        assert_eq!(generated, expected);
    };
    ($generate_from:literal, $expected:literal) => {
        compare_foos!(impl_::Args::default(), $generate_from, $expected)
    };
}

macro_rules! get_interface {
        ($($tt:tt)*) => {
            impl_::local_slots_interface(syn::parse_quote!($($tt)*)).unwrap()
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
                type foo_{type_suffix};
                fn foo_{init_suffix}<{init_context_type}>(init_context: &mut {init_context_type}) -> self:: foo_{type_suffix};
                fn foo_{enter_suffix}({context}: &mut self:: foo_{type_suffix});
            "
        );
}

#[test]
fn init_context_bounds() {
    compare_foos!(
            impl_::Args{
                bounds: syn::parse_quote! { AsRef<str> + AsRef<u8> },
                ..Default::default()
            },
            "
                fn foo();
            ",
            "
                #[allow(non_camel_case_types)]
                type foo_{type_suffix};
                fn foo_{init_suffix}<{init_context_type}: AsRef<str> + AsRef<u8>>(init_context: &mut {init_context_type}) -> self:: foo_{type_suffix};
                fn foo_{enter_suffix}({context}: &mut self:: foo_{type_suffix});
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
                type foo_{type_suffix} = (u8, u8, u8, foo_{type_suffix}, foo_{type_suffix},);
                fn foo_{init_suffix}<{init_context_type}>(init_context: &mut {init_context_type}) -> self:: foo_{type_suffix} {{
                    (3, 2, 1, foo_{init_suffix}(init_context), foo_{init_suffix}(init_context),)
                }}
                fn foo_{enter_suffix}({context}: &mut self:: foo_{type_suffix}) {{
                    let ref mut x: u8 = {context}.0;
                    &mut {context}.1;
                    &mut {context}.2;

                    foo_{enter_suffix}(&mut {context}.3);
                    foo_{enter_suffix}(&mut {context}.4);
                }}
            "
        );
}
