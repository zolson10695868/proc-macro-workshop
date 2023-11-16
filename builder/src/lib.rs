use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Fields, GenericArgument,
    Ident, Path, PathArguments, PathSegment, Type, TypePath,  Expr, Lit, ExprLit, ExprAssign, ExprPath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let id = &input.ident;
    let builder_name = format_ident!("{id}Builder");
    let fields = &input.data;
    let Data::Struct(fields) = fields else {
        unimplemented!();
    };
    let Fields::Named(fields) = &fields.fields else {
        unimplemented!()
    };
    let (ids, (types, each)): (Vec<_>, (Vec<_>, Vec<_>)) = fields
        .named
        .iter()
        .map(|f| (f.ident.as_ref().unwrap(), (TypeStatus::new(f.ty.clone()), f.attrs.iter().find_map(|at| { 
            if !at.path().is_ident("builder") {
                return None;
            }

            let set: Expr = at.parse_args().unwrap();
            let Expr::Assign(ExprAssign {left, right, ..}) = set else {
                return None
            };
            let Expr::Path(ExprPath { path, .. }) = *left else {
                return None
            };
            if !path.is_ident("each") {
                return Some(Err(syn::Error::new_spanned(&at.meta, "expected `builder(each = \"...\")`")))
            };
            let Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) = *right else {
                return None
            };
            Some(Ok(format_ident!("{}", s.value())))

        }))))
        .unzip();
    let each: Vec<Option<_>> = match each.into_iter().map(|o| o.transpose()).collect::<Result<_, _>>() {
        Ok(e) => e,
        Err(e) => return  e.into_compile_error().into()   };
    let req = types
        .iter().zip(&each)
        .map(|(ty, e)| matches!(ty, TypeStatus::Required(..)) && e.is_none())
        .collect::<Vec<_>>();
    let types = types
        .into_iter()
        .map(|ty| match ty {
            TypeStatus::Required(t) | TypeStatus::Optional(t) => t,
        })
        .collect::<Vec<_>>();
    let fns = ids.iter().zip(&types).zip(&each).map(|((id, ty), each)| setter_fns(id, each.as_ref(), ty));
    let assigns = ids
        .iter()
        .zip(&req).zip(&each)
        .map(|((i, r), e)| assign_builder(i, *r, e.as_ref()))
        .collect::<Vec<_>>();
    quote! {
        impl #id {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }

        #[derive(Default)]
        pub struct #builder_name {
            #(#ids: ::std::option::Option<#types>,)*
        }

        impl #builder_name {
            pub fn build(&mut self) -> ::std::result::Result<#id, ::std::boxed::Box<dyn ::std::error::Error + 'static>> {
                #(
                   #assigns 
                 )*

                Ok(#id {
                    #(#ids,)*
                })
            }
            #(#fns)*
        }
    }
    .into()
}

fn assign_builder(id: &Ident, req: bool, e: Option<&Ident>) -> impl ToTokens {
    if req && e.is_none() {
        quote! {
            let #id = self.#id.take().ok_or_else(|| format!("missing {}", stringify!(#id)))?;
        }
    } else if e.is_some() {
        quote!{
            let #id = self.#id.take().unwrap_or_default();
        }
    } else  {
        quote! {
            let #id = self.#id.take();
        }
    }
}

fn setter_fns(id: &Ident, each_name: Option<&Ident>, ty: &Type) -> impl ToTokens {
    let raw = quote! {
        pub fn #id(&mut self, #id: #ty) -> &mut Self {
            self.#id = Some(#id);
            self
        }
    };
    let each = each_name.map(|each|{
        let ty = inner(ty);
        quote! {
            pub fn #each(&mut self, #each: #ty) -> &mut Self {
                self.#id.get_or_insert_with(Vec::new).push(#each);
                self
            }
        }}
    );
    if each_name == Some(id) {
        quote! { #each }
    } else {
        quote! {
            #each

            #raw
        }
    }
}

enum TypeStatus {
    Required(Type),
    Optional(Type),
}

impl TypeStatus {
    fn new(ty: Type) -> Self {
        match &ty {
            Type::Path(TypePath {
                qself: None,
                path: Path { segments, .. },
            }) => {
                let Some(PathSegment { ident, arguments }) = segments.iter().next() else {
                    return Self::Required(ty);
                };
                if ident != "Option" {
                    return Self::Required(ty);
                }
                match arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => Self::Optional(match args.iter().next().unwrap() {
                        GenericArgument::Type(ty) => ty.clone(),
                        _ => unimplemented!(),
                    }),
                    _ => Self::Required(ty),
                }
            }
            _ => Self::Required(ty),
        }
    }
}

fn inner(ty: &Type) -> Type {
        match ty {
            Type::Path(TypePath {
                qself: None,
                path: Path { segments, .. },
            }) => {
                let Some(PathSegment { ident, arguments }) = segments.iter().next() else {
                    unimplemented!()
                };
                if ident != "Vec" {
                    unimplemented!()
                }
                match arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => match args.iter().next().unwrap() {
                        GenericArgument::Type(ty) => ty.clone(),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }

}
