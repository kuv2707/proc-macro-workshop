extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, MetaList};
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };
    let name = &ast.ident;
    let builder_name = &format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());

    let optionized_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = unwrap_type(&f.ty, "Option".into());
        let builderfname = builder_func_name(f);
        match builderfname {
            Some(_) => {
                // a field having the builder attribute is assumed to be a Vec
                quote! {
                    #name: #ty
                }
            }
            None => {
                quote! {
                    #name: std::option::Option<#ty>
                }
            }
        }
    });
    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        let bfname = builder_func_name(f);
        if bfname.is_some() {
            quote! {
                #name: vec![]
            }
        } else {
            quote! {
                #name: std::option::Option::None
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = unwrap_type(&f.ty, "Option".into());
        let builderfname = builder_func_name(f);
        match builderfname {
            Some(fname) => {
                if fname.0 == "" {
                    return syn::Error::new_spanned(fname.1, "expected `builder(each = \"...\")`")
                        .to_compile_error()
                        .into();
                }
                let ty = unwrap_type(ty, "Vec".into());
                let fname = syn::Ident::new(&fname.0, name.span());
                quote! {
                    fn #fname(&mut self, #fname: #ty) -> &mut Self {
                        self.#name.push(#fname);
                        self
                    }
                }
            }
            None => {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if is_option(&f.ty) || builder_func_name(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or("missing field")?
            }
        }
    });
    let ts = quote! {
        struct #builder_ident {
            #(#optionized_fields,)*
        }
        impl #builder_ident {
            #(#methods)*
            fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
        impl #name {
            fn builder() -> #builder_ident{
                #builder_ident {
                    #(#build_empty,)*
                }
            }
        }
    };
    ts.into()
}

// to check for option, we should ideally check more than whether the last element
// is "Option". Pero esto basta para ahora.
fn is_option(ty: &syn::Type) -> bool {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "Option" {
                return true;
            }
        }
    }
    false
}

fn unwrap_type(ty: &syn::Type, datatype: String) -> &syn::Type {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident.to_string() == datatype {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = &segment.arguments
                {
                    if let syn::GenericArgument::Type(ty) = args.first().unwrap() {
                        return ty;
                    }
                }
            }
        }
    }
    ty
}

fn builder_func_name(f: &syn::Field) -> Option<(String, MetaList)> {
    let attr = f.attrs.iter().find(|attr| attr.path().is_ident("builder"));
    if attr.is_none() {
        return None;
    }
    let attr = attr.unwrap();

    if attr.path().segments[0].ident == "builder" {
        if let syn::Meta::List(k) = &attr.meta {
            let toks = k
                .tokens
                .clone()
                .into_iter()
                .map(|k| k.to_string())
                .collect::<std::vec::Vec<String>>();
            if toks.len() != 3 || toks[0] != "each" || toks[1] != "=" {
                return Some(("".into(), k.clone()));
            }

            let fnnametoken = k.tokens.clone().into_iter().last().unwrap().to_string();
            let fnname = fnnametoken.trim_matches('"');

            return Some((fnname.into(), k.clone()));
        }
    }
    None
}
