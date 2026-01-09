use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DeriveInput, Error, Expr, Field, Fields, GenericArgument, Ident, Lit,
    MetaNameValue, PathArguments, Result, Type,
};

enum BuilderField {
    Required {
        name: Ident,
        ty: Type,
    },
    Optional {
        name: Ident,
        ty: Type,
    },
    Builder {
        name: Ident,
        method: Ident,
        ty: Type,
    },
    OnlyBuilder {
        name: Ident,
        ty: Type,
    },
}
impl BuilderField {
    fn quote_field(&self) -> TokenStream2 {
        match self {
            BuilderField::Required { name, ty } | BuilderField::Optional { name, ty } => quote! {
                #name: std::option::Option<#ty>
            },
            BuilderField::Builder { name, ty, .. } | BuilderField::OnlyBuilder { name, ty } => {
                quote! {
                    #name: std::vec::Vec<#ty>
                }
            }
        }
    }

    fn quote_initialization(&self) -> TokenStream2 {
        match self {
            BuilderField::Required { name, .. } | BuilderField::Optional { name, .. } => quote! {
                #name: None
            },
            BuilderField::Builder { name, .. } | BuilderField::OnlyBuilder { name, .. } => quote! {
                #name: std::vec::Vec::new()
            },
        }
    }

    fn quote_methods(&self) -> TokenStream2 {
        let set_method = match self {
            BuilderField::Required { name, ty } | BuilderField::Optional { name, ty } => quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            BuilderField::Builder { name, ty, .. } => quote! {
                fn #name(&mut self, #name: Vec<#ty>) -> &mut Self {
                    self.#name = #name;
                    self
                }
            },
            _ => TokenStream2::new(),
        };

        let append_method = match self {
            BuilderField::Builder { name, method, ty } => quote! {
                fn #method(&mut self, value: #ty) -> &mut Self {
                    self.#name.push(value);
                    self
                }
            },
            BuilderField::OnlyBuilder { name, ty } => quote! {
                fn #name(&mut self, value: #ty) -> &mut Self {
                    self.#name.push(value);
                    self
                }
            },
            _ => TokenStream2::new(),
        };

        quote! {
            #set_method
            #append_method
        }
    }

    fn quote_build(&self) -> TokenStream2 {
        match self {
            BuilderField::Required { name, .. } => {
                let error_message = format!("{} must be set", name);
                quote! {
                    #name: self.#name.take().ok_or(#error_message)?
                }
            }
            BuilderField::Optional { name, .. } => quote! {
                #name: self.#name.take()
            },
            BuilderField::Builder { name, .. } | BuilderField::OnlyBuilder { name, .. } => quote! {
                #name: std::mem::take(&mut self.#name)
            },
        }
    }
}

fn get_builder_attribute(field: &Field) -> Result<Option<Ident>> {
    field
        .attrs
        .iter()
        .find(|attribute| attribute.path().is_ident("builder"))
        .map(|attribute| {
            attribute
                .parse_args::<MetaNameValue>()
                .and_then(|name_value| {
                    if name_value.path.is_ident("each") {
                        if let Expr::Lit(expr_lit) = &name_value.value {
                            if let Lit::Str(lit) = &expr_lit.lit {
                                return Ok(Ident::new(&lit.value(), Span::call_site()));
                            }
                        }
                    }
                    Err(Error::new_spanned(
                        &attribute.meta,
                        "expected `builder(each = \"...\")`",
                    ))
                })
        })
        .transpose()
}

fn get_argument_type(ty: &Type, ident: &str) -> Option<Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };
    let Some(path_segment) = type_path.path.segments.first() else {
        return None;
    };
    let PathArguments::AngleBracketed(arguments) = &path_segment.arguments else {
        return None;
    };
    let Some(GenericArgument::Type(argument_type)) = arguments.args.first() else {
        return None;
    };

    if path_segment.ident == ident
        && type_path.path.segments.len() == 1
        && arguments.args.len() == 1
    {
        Some(argument_type.clone())
    } else {
        None
    }
}

impl TryFrom<&Field> for BuilderField {
    type Error = Error;

    fn try_from(field: &Field) -> std::result::Result<Self, Self::Error> {
        let name = field
            .ident
            .clone()
            .ok_or(Error::new_spanned(&field, "must have an ident"))?;
        Ok(if let Some(method) = get_builder_attribute(field)? {
            let ty = get_argument_type(&field.ty, "Vec")
                .ok_or(Error::new_spanned(&field.ty, "must be a vec"))?;
            if name == method {
                BuilderField::OnlyBuilder { name, ty }
            } else {
                BuilderField::Builder { name, method, ty }
            }
        } else if let Some(ty) = get_argument_type(&field.ty, "Option") {
            BuilderField::Optional { name, ty }
        } else {
            BuilderField::Required {
                name,
                ty: field.ty.clone(),
            }
        })
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);

    let field_parse_result: Result<Vec<BuilderField>> = match input.data {
        Data::Struct(data_struct) => match data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .into_iter()
                .map(|field| BuilderField::try_from(&field)),
            Fields::Unnamed(_) => unimplemented!(),
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) => unimplemented!(),
        Data::Union(_) => unimplemented!(),
    }
    .collect();

    TokenStream::from(match field_parse_result {
        Ok(fields) => {
            let field_initializers = fields.iter().map(|field| field.quote_initialization());
            let builder_fields = fields.iter().map(|field| field.quote_field());
            let builder_methods = fields.iter().map(|field| field.quote_methods());
            let builder_build = fields.iter().map(|field| field.quote_build());

            quote! {
                impl #name {
                    pub fn builder() -> #builder_name {
                        #builder_name {
                            #(#field_initializers),*
                        }
                    }
                }

                pub struct #builder_name {
                    #(#builder_fields),*
                }

                impl #builder_name {
                    #(#builder_methods)*

                    pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                        std::result::Result::Ok(#name {
                            #(#builder_build),*
                        })
                    }
                }
            }
        }
        Err(error) => error.into_compile_error(),
    })
}
