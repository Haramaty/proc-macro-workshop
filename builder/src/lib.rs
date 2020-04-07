extern crate proc_macro;
use std::convert::TryFrom;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as Quote;
use quote::{format_ident, quote};
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let building = parse_macro_input!(input as syn::DeriveInput);
    let builder = format_ident!("{}Builder", &building.ident);

    let building_impl = building_impl(&building.vis, &building.generics, &building.ident, &builder);

    let fields = match building.data {
        syn::Data::Struct(data) => data.fields,
        _ => unimplemented!(),
    };
    let builder_quote = match named_struct(
        &builder,
        &building.ident,
        &building.vis,
        &building.generics,
        fields,
    ) {
        Ok(builder_quote) => builder_quote,
        _ => unimplemented!(),
    };

    let output = quote! {
        #builder_quote
        #building_impl
    };

    output.into()
}

enum Error {
    NotSupported,
}

fn building_impl(
    vis: &syn::Visibility,
    generics: &syn::Generics,
    building: &syn::Ident,
    builder: &syn::Ident,
) -> Quote {
    quote! {
        impl #generics #building #generics {
            #vis fn builder() -> #builder #generics {
                <#builder #generics>::new()
            }
        }
    }
}

fn named_struct(
    builder: &syn::Ident,
    building: &syn::Ident,
    vis: &syn::Visibility,
    generics: &syn::Generics,
    fields: syn::Fields,
) -> Result<Quote, Error> {
    let fields: Result<Vec<_>, Error> = fields.into_iter().map(Field::try_from).collect();
    let (optional, mandatory): (Vec<_>, Vec<_>) = fields?.into_iter().partition(Field::is_optional);

    let (opt_ident, opt_ty): (Vec<_>, Vec<_>) = optional.iter().map(|f| (&f.ident, &f.ty)).unzip();
    let (mand_ident, mand_ty): (Vec<_>, Vec<_>) =
        mandatory.iter().map(|f| (&f.ident, &f.ty)).unzip();

    Ok(quote! {
        #[derive(Debug)]
        #vis struct #builder #generics {
            #( #mand_ident: ::std::option::Option<#mand_ty>, )*
            #( #opt_ident: ::std::option::Option<#opt_ty>, )*
        }

        impl #generics #builder #generics {
            pub fn new() -> Self {
                <Self as ::std::default::Default>::default()
            }

            #(
                pub fn #mand_ident(&mut self, #mand_ident: #mand_ty) -> &mut Self {
                    self. #mand_ident = Some(#mand_ident);
                    self
                }
            )*

            #(
                pub fn #opt_ident(&mut self, #opt_ident: #opt_ty) -> &mut Self {
                    self. #opt_ident = Some(#opt_ident);
                    self
                }
            )*

            pub fn build(&self) -> ::std::result::Result<#building #generics, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#building {
                    #( #mand_ident: (&self. #mand_ident ).as_ref().ok_or("oh no!")?.clone(), )*
                    #( #opt_ident: self. #opt_ident .clone(), )*
                })
            }
        }

        impl #generics ::std::default::Default for #builder #generics {
            fn default() -> Self {
                Self {
                    #( #mand_ident: None, )*
                    #( #opt_ident: None, )*
                }
            }
        }
    })
}

#[derive(Debug)]
struct Field {
    ident: syn::Ident,
    ty: syn::Type,
    is_optional: bool,
}

impl Field {
    pub fn is_optional(&self) -> bool {
        self.is_optional
    }

    fn calculate_optional(ty: syn::Type) -> (syn::Type, bool) {
        let orig_ty = ty;

        let ty = match &orig_ty {
            syn::Type::Path(ty) if ty.qself.is_none() => &ty.path,
            _ => return (orig_ty, false),
        };

        if ty.leading_colon.is_some() || ty.segments.len() != 1 {
            return (orig_ty, false);
        };
        let ty = ty.segments.first().unwrap();

        if ty.ident != format_ident!("Option") {
            return (orig_ty, false);
        };

        let args = match &ty.arguments {
            syn::PathArguments::AngleBracketed(arguments) if arguments.args.len() == 1 => {
                arguments.args.first().unwrap()
            }
            _ => return (orig_ty, false),
        };

        match args {
            syn::GenericArgument::Type(ty) => (ty.clone(), true),
            _ => (orig_ty, false),
        }
    }
}

impl TryFrom<syn::Field> for Field {
    type Error = Error;

    fn try_from(field: syn::Field) -> Result<Self, Self::Error> {
        let ident = field.ident.ok_or(Error::NotSupported)?;
        let (ty, is_optional) = Field::calculate_optional(field.ty);

        Ok(Field {
            ident,
            ty,
            is_optional,
        })
    }
}
