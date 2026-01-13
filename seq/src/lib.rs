use std::ops::Range;

use proc_macro::TokenStream;
use proc_macro2::{
    token_stream::IntoIter, Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree,
};
use syn::{
    Ident, LitInt, Result, Token, braced, parse::{Parse, ParseStream}, parse_macro_input
};

struct Sequence {
    ident: Ident,
    range: Range<usize>,
    content: TokenStream2,
}

impl Sequence {
    fn expand(self) -> Result<TokenStream2> {
        match SectionExpandIterator::try_from(self) {
            Ok(iterator) => iterator.collect(),
            Err(sequence) => ExpandIterator::from(sequence).collect(),
        }
    }
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start: usize = input.parse::<LitInt>()?.base10_parse()?;

        let inclusive = input.peek(Token![..=]);
        if inclusive {
            input.parse::<Token![..=]>()?;
        } else {
            input.parse::<Token![..]>()?;
        }

        let end: usize = input.parse::<LitInt>()?.base10_parse()?;
        let range = if inclusive {
            start..end + 1
        } else {
             start..end
        };

        let content;
        braced!(content in input);

        Ok(Sequence {
            ident,
            range,
            content: content.parse()?,
        })
    }
}

struct ExpandIterator {
    ident: Ident,
    tokens: TokenStream2,
    range: Range<usize>,
    iteration: Option<IdentReplacementIterator<IntoIter>>,
}

impl From<Sequence> for ExpandIterator {
    fn from(sequence: Sequence) -> Self {
        ExpandIterator {
            ident: sequence.ident,
            tokens: sequence.content,
            range: sequence.range,
            iteration: None,
        }
    }
}

impl Iterator for ExpandIterator {
    type Item = Result<TokenTree>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iteration
            .as_mut()
            .and_then(|iterator| iterator.next())
            .or_else(|| {
                self.range.next()
                    .map(|value| {
                        let mut literal = Literal::usize_unsuffixed(value);
                        literal.set_span(self.ident.span());
                        literal
                    })
                    .and_then(|literal| {
                        self.iteration = Some(IdentReplacementIterator {
                            ident: self.ident.clone(),
                            literal,
                            tokens: self.tokens.clone().into_iter(),
                        });
                        self.next()
                    })
            })
    }
}

fn has_section(tokens: TokenStream2) -> bool {
    let mut iterator = tokens.into_iter();
    loop {
        match iterator.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                let mut lookahead = iterator.clone();
                match (lookahead.next(), lookahead.next()) {
                    (Some(TokenTree::Group(group)), Some(TokenTree::Punct(punct)))
                        if group.delimiter() == Delimiter::Parenthesis
                            && punct.as_char() == '*' =>
                    {
                        break true;
                    }
                    _ => (),
                }
            }
            Some(TokenTree::Group(group)) => {
                if has_section(group.stream()) {
                    break true;
                }
            }
            None => {
                break false;
            }
            _ => (),
        }
    }
}

struct SectionExpandIterator<I>
where
    I: Iterator<Item = TokenTree> + Clone,
{
    ident: Ident,
    tokens: I,
    range: Range<usize>,
    section: Option<ExpandIterator>,
}

impl TryFrom<Sequence> for SectionExpandIterator<IntoIter> {
    type Error = Sequence;

    fn try_from(sequence: Sequence) -> std::result::Result<Self, Self::Error> {
        if has_section(sequence.content.clone()) {
            Ok(SectionExpandIterator {
                ident: sequence.ident,
                tokens: sequence.content.into_iter(),
                range: sequence.range,
                section: None,
            })
        } else {
            Err(sequence)
        }
    }
}

impl<I> Iterator for SectionExpandIterator<I>
where
    I: Iterator<Item = TokenTree> + Clone,
{
    type Item = Result<TokenTree>;

    fn next(&mut self) -> Option<Self::Item> {
        self.section
            .as_mut()
            .and_then(|iterator| iterator.next())
            .or_else(|| match self.tokens.next() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                    let mut lookahead = self.tokens.clone();
                    match (lookahead.next(), lookahead.next()) {
                        (Some(TokenTree::Group(group)), Some(TokenTree::Punct(punct)))
                            if group.delimiter() == Delimiter::Parenthesis
                                && punct.as_char() == '*' =>
                        {
                            self.tokens.nth(2);
                            let iterator = ExpandIterator {
                                ident: self.ident.clone(),
                                tokens: group.stream(),
                                range: self.range.clone(),
                                iteration: None,
                            };
                            self.section = Some(iterator);
                            self.next()
                        }
                        _ => Some(Ok(TokenTree::Punct(punct))),
                    }
                }
                Some(TokenTree::Group(group)) => {
                    let iterator = SectionExpandIterator {
                        ident: self.ident.clone(),
                        tokens: group.stream().into_iter(),
                        range: self.range.clone(),
                        section: None,
                    };
                    match iterator.collect::<Result<TokenStream2>>() {
                        Ok(tokens) => {
                            let mut new_group = Group::new(group.delimiter(), tokens);
                            new_group.set_span(group.span());
                            Some(Ok(TokenTree::Group(new_group)))
                        }
                        Err(error) => Some(Err(error)),
                    }
                }
                Some(token) => Some(Ok(token)),
                None => None,
            })
    }
}

struct IdentReplacementIterator<I>
where
    I: Iterator<Item = TokenTree> + Clone,
{
    ident: Ident,
    literal: Literal,
    tokens: I,
}

impl<'a, I> Iterator for IdentReplacementIterator<I>
where
    I: Iterator<Item = TokenTree> + Clone,
{
    type Item = Result<TokenTree>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.next() {
            Some(token) => match &token {
                TokenTree::Group(group) => {
                    let iterator = IdentReplacementIterator {
                        ident: self.ident.clone(),
                        literal: self.literal.clone(),
                        tokens: group.stream().into_iter(),
                    };
                    match iterator.collect::<Result<TokenStream2>>() {
                        Ok(tokens) => {
                            let mut new_group = Group::new(group.delimiter(), tokens);
                            new_group.set_span(group.span());
                            Some(Ok(TokenTree::Group(new_group)))
                        }
                        Err(error) => Some(Err(error)),
                    }
                }
                TokenTree::Ident(ident) if *ident == self.ident => {
                    Some(Ok(TokenTree::Literal(self.literal.clone())))
                }
                TokenTree::Ident(prefix) => {
                    let mut lookahead = self.tokens.clone();
                    match (
                        &lookahead.next(),
                        &lookahead.next(),
                        &lookahead.next(),
                        &lookahead.next(),
                    ) {
                        (
                            Some(TokenTree::Punct(prefix_punct)),
                            Some(TokenTree::Ident(ident)),
                            Some(TokenTree::Punct(suffix_punct)),
                            Some(TokenTree::Ident(suffix)),
                        ) if *ident == self.ident
                            && prefix_punct.as_char() == '~'
                            && suffix_punct.as_char() == '~' =>
                        {
                            let concat_ident = format!("{}{}{}", prefix, self.literal, suffix);
                            self.tokens.nth(3);
                            Some(Ok(TokenTree::Ident(Ident::new(
                                &concat_ident,
                                prefix.span(),
                            ))))
                        }
                        (
                            Some(TokenTree::Punct(prefix_punct)),
                            Some(TokenTree::Ident(ident)),
                            _,
                            _,
                        ) if *ident == self.ident && prefix_punct.as_char() == '~' => {
                            let concat_ident = format!("{}{}", prefix, self.literal);
                            self.tokens.nth(1);
                            Some(Ok(TokenTree::Ident(Ident::new(
                                &concat_ident,
                                prefix.span(),
                            ))))
                        }
                        _ => Some(Ok(token)),
                    }
                }
                _ => Some(Ok(token)),
            },
            None => None,
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let sequence = parse_macro_input!(input as Sequence);

    TokenStream::from(match sequence.expand() {
        Ok(tokens) => tokens,
        Err(error) => error.into_compile_error(),
    })
}
