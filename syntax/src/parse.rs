use chumsky::{
    primitive::{choice, end, just},
    recursive::recursive,
    select, Parser,
};

use crate::{
    ast,
    error::{Error, Pattern},
    node::SrcNode,
    token::{Delimiter, Token},
};

pub mod helper {
    use chumsky::{primitive::just, recovery::nested_delimiters};

    use crate::{
        error,
        span::Span,
        token::{Delimiter, Token},
    };

    pub trait Parser<T> = chumsky::Parser<Token, T, Error = error::Error> + Clone;

    pub type BoxedParser<'a, T> = chumsky::BoxedParser<'a, Token, T, error::Error>;

    pub fn nested_parser<'a, T, P, F>(parser: P, delimiter: Delimiter, f: F) -> BoxedParser<'a, T>
    where
        T: 'a,
        P: Parser<T> + 'a,
        F: Fn(Span) -> T + Clone + 'a,
    {
        parser
            .delimited_by(just(Token::Open(delimiter)), just(Token::Close(delimiter)))
            .recover_with(nested_delimiters(
                Token::Open(delimiter),
                Token::Close(delimiter),
                [(
                    Token::Open(Delimiter::Paren),
                    Token::Close(Delimiter::Paren),
                )],
                f,
            ))
            .boxed()
    }
}

use helper::{nested_parser, BoxedParser};

pub fn ident_parser() -> impl helper::Parser<ast::Ident> {
    select! {
        Token::Ident(id) => id,
    }
    .map_err(|e: Error| e.expected(Pattern::Ident))
}

pub fn lit_parser() -> impl helper::Parser<ast::Lit> {
    select! {
        Token::Int(int) => ast::Lit::Int(int),
        Token::Str(r#str) => ast::Lit::Str(r#str),
    }
    .map_err(|e: Error| e.expected(Pattern::Literal))
}

pub fn path_parser() -> impl helper::Parser<ast::Path> {
    ident_parser()
        .map_with_span(SrcNode::new)
        .separated_by(just([Token::Colon, Token::Colon]))
        .at_least(1)
        .allow_leading()
        .map(|segments| ast::Path { segments })
}

pub fn ty_parser() -> impl helper::Parser<ast::Ty> {
    recursive(|ty| {
        let path =
            path_parser().map_with_span(|path, span| ast::Ty::Path(SrcNode::new(path, span)));

        let ptr = just::<_, _, Error>(Token::Star)
            .ignore_then(ty)
            .map_with_span(|ty, span| ast::Ty::Ptr(SrcNode::new(ty, span)));

        path.or(ptr)
    })
}

pub fn expr_parser() -> impl helper::Parser<ast::Expr> {
    recursive(|expr| {
        let paren_expr_list: BoxedParser<Option<Vec<SrcNode<ast::Expr>>>> = nested_parser(
            expr.clone()
                .map_with_span(SrcNode::new)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .map(Some),
            Delimiter::Paren,
            |_| None,
        );

        let lit = lit_parser().map_with_span(|lit, span| ast::Expr::Lit(SrcNode::new(lit, span)));
        let path =
            path_parser().map_with_span(|path, span| ast::Expr::Path(SrcNode::new(path, span)));

        let atom: BoxedParser<SrcNode<ast::Expr>> = lit
            .or(path)
            .or(select! { Token::Error(_) => () }
                .map(|_| ast::Expr::Error)
                .boxed())
            .map_with_span(SrcNode::new)
            .boxed();

        let call: BoxedParser<SrcNode<ast::Expr>> = atom
            .then(paren_expr_list.or_not())
            .map_with_span(|(expr, args), span| match args {
                Some(Some(args)) => {
                    let span = args.iter().fold(span, |span, arg| span.union(arg.span()));

                    SrcNode::new(ast::Expr::Call(expr, args), span)
                }
                Some(None) => SrcNode::new(ast::Expr::Error, span),
                None => expr,
            })
            .boxed();

        let op = just(Token::And).map_with_span(SrcNode::new);
        let addr = op
            .repeated()
            .then(call)
            .foldr(|op, expr| {
                let span = op.span().union(expr.span());
                SrcNode::new(ast::Expr::Addr(expr), span)
            })
            .boxed();

        let op = just(Token::Star)
            .to(ast::UnOp::Deref)
            .or(just(Token::Minus).to(ast::UnOp::Neg))
            .map_with_span(SrcNode::new);
        let unary = op
            .repeated()
            .then(addr)
            .foldr(|op, expr| {
                let span = op.span().union(expr.span());
                SrcNode::new(ast::Expr::Unary(op, expr), span)
            })
            .boxed();

        let op = just(Token::Star)
            .to(ast::BinOp::Mul)
            .or(just(Token::Slash).to(ast::BinOp::Div))
            .or(just(Token::Percent).to(ast::BinOp::Rem))
            .map_with_span(SrcNode::new);
        let product = unary
            .clone()
            .then(op.then(unary).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        let op = just(Token::Plus)
            .to(ast::BinOp::Add)
            .or(just(Token::Minus).to(ast::BinOp::Sub))
            .map_with_span(SrcNode::new);
        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        sum.map(SrcNode::into_inner)
    })
}

// TODO: move out parsers for specific item/stmt kinds into
// their own functions for cleaner unit testing.
pub fn item_parser() -> impl helper::Parser<ast::Item> {
    recursive(|item| {
        let init: BoxedParser<ast::Local> = ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Colon))
            .then(ty_parser().map_with_span(SrcNode::new).or_not())
            .then_ignore(just(Token::Eq))
            .then(expr_parser().map_with_span(SrcNode::new))
            .map_with_span(|((ident, ty), expr), span| {
                let kind = ast::LocalKind::Init(expr, ty);

                ast::Local {
                    ident,
                    kind: SrcNode::new(kind, span),
                }
            })
            .boxed();
        let decl = ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Colon))
            .then(ty_parser().map_with_span(SrcNode::new))
            .map_with_span(|(ident, ty), span| {
                let kind = ast::LocalKind::Decl(ty);

                ast::Local {
                    ident,
                    kind: SrcNode::new(kind, span),
                }
            })
            .boxed();
        let local = init.or(decl).boxed();

        let stmt = choice((
            item.map_with_span(|item, span| ast::Stmt::Item(SrcNode::new(item, span))),
            expr_parser()
                .map_with_span(SrcNode::new)
                .map(ast::Stmt::Expr)
                .then_ignore(just(Token::Semicolon)),
            local
                .map_with_span(|local, span| ast::Stmt::Local(SrcNode::new(local, span)))
                .then_ignore(just(Token::Semicolon)),
        ))
        .boxed();

        let block = stmt
            .repeated()
            .delimited_by(
                just(Token::Open(Delimiter::Brace)),
                just(Token::Close(Delimiter::Brace)),
            )
            .map_with_span(|stmts, span| SrcNode::new(ast::Block { stmts }, span))
            .boxed();

        let generics: BoxedParser<Option<SrcNode<ast::Generics>>> = ident_parser()
            .map_with_span(SrcNode::new)
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::Lt), just(Token::Gt))
            .map_with_span(|params, span| SrcNode::new(ast::Generics { params }, span))
            .or_not()
            .boxed();

        let field = ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Colon))
            .then(ty_parser().map_with_span(SrcNode::new))
            .map_with_span(|(ident, ty), span| SrcNode::new(ast::FieldDef { ident, ty }, span))
            .boxed();
        let fields = field
            .repeated()
            .delimited_by(
                just(Token::Open(Delimiter::Brace)),
                just(Token::Close(Delimiter::Brace)),
            )
            .map_with_span(SrcNode::new)
            .boxed();

        let r#struct = just(Token::Struct)
            .ignore_then(ident_parser().map_with_span(SrcNode::new))
            .then(generics.clone())
            .then(fields.map(Some).or(just(Token::Semicolon).to(None)))
            .map_with_span(|((ident, generics), fields), span| {
                let r#struct = ast::Struct { generics, fields };
                let kind = ast::ItemKind::Struct(r#struct);

                ast::Item {
                    ident,
                    kind: SrcNode::new(kind, span),
                }
            })
            .boxed();

        let param = ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Colon))
            .then(ty_parser().map_with_span(SrcNode::new))
            .map_with_span(|(ident, ty), span| SrcNode::new(ast::Param { ident, ty }, span))
            .boxed();
        let params = param
            .separated_by(just(Token::Comma))
            .delimited_by(
                just(Token::Open(Delimiter::Paren)),
                just(Token::Close(Delimiter::Paren)),
            )
            .map_with_span(SrcNode::new)
            .boxed();

        let ret_ty = just(Token::RArrow)
            .ignore_then(ty_parser())
            .or_not()
            .map_with_span(|ty, span| match ty {
                Some(ty) => ast::FnRetTy::Ty(SrcNode::new(ty, span)),
                // TODO: Figure out why span is wrong
                // i.e: 62..61 instead of 61..62
                None => ast::FnRetTy::Default(SrcNode::new((), span)),
            })
            .map_with_span(SrcNode::new)
            .boxed();

        let func = just(Token::Func)
            .ignore_then(ident_parser().map_with_span(SrcNode::new))
            .then(generics.clone())
            .then(params)
            .then(ret_ty)
            .then(block)
            .map_with_span(|((((ident, generics), inputs), output), block), span| {
                let sig = ast::FnSig { inputs, output };
                let func = ast::Func {
                    generics,
                    sig,
                    block,
                };
                let kind = ast::ItemKind::Func(func);

                ast::Item {
                    ident,
                    kind: SrcNode::new(kind, span),
                }
            })
            .boxed();

        r#struct.or(func)
    })
}

pub fn module_parser() -> impl helper::Parser<ast::Module> {
    item_parser()
        .map_with_span(|item, span| SrcNode::new(item, span))
        .repeated()
        .then_ignore(end())
        .map(|items| ast::Module { items })
}

#[cfg(test)]
mod tests {
    use chumsky::{primitive::end, Parser, Span};

    use crate::{
        ast,
        parse::{expr_parser, ident_parser, item_parser, lit_parser, path_parser, ty_parser},
    };

    macro_rules! SN {
        [$t:expr, $start:literal, $end:literal] => {{
            let span = $crate::span::Span::new($crate::src::SrcId::empty(), $start..$end);
            $crate::node::SrcNode::new($t, span)
        }};
    }

    macro_rules! Id {
        ($id:ident) => {
            $crate::ast::Ident::new(stringify!($id))
        };
        ($id:literal) => {
            $crate::ast::Ident::new($id)
        };
    }

    macro_rules! Lit {
        ($lit:literal int) => {
            $crate::ast::Lit::Int($lit)
        };
    }

    // TODO: Add unit tests for parse errors
    macro_rules! expect_parse {
        ($code:literal, $parser:ident, $ast:expr) => {
            let code = $code;
            let len = code.chars().count();

            let span = |i| $crate::span::Span::new($crate::src::SrcId::empty(), i..i + 1);
            let tokens = $crate::token::lexer()
                .parse(chumsky::Stream::from_iter(
                    span(len),
                    code.chars().enumerate().map(|(i, c)| (c, span(i))),
                ))
                .unwrap();

            let ast = $parser()
                .then_ignore(end())
                .parse(chumsky::Stream::from_iter(span(len), tokens.into_iter()));

            assert!(ast.is_ok());
            let ast = ast.unwrap();

            assert_eq!(ast, $ast);
        };
    }

    #[test]
    fn parse_ident() {
        expect_parse!("num", ident_parser, Id![num]);
    }

    #[test]
    fn parse_lit() {
        expect_parse!("1", lit_parser, Lit![1 int]);
    }

    #[test]
    fn parse_path() {
        expect_parse!(
            "std::array",
            path_parser,
            ast::Path {
                segments: vec![SN![Id![std], 0, 3], SN![Id![array], 5, 10]]
            }
        );
    }

    #[test]
    fn parse_ty() {
        expect_parse!(
            "T",
            ty_parser,
            ast::Ty::Path(SN![
                ast::Path {
                    segments: vec![SN![Id![T], 0, 1]]
                },
                0,
                1
            ])
        );
    }

    #[test]
    fn parse_expr() {
        expect_parse!(
            "1 + f(*&2)",
            expr_parser,
            ast::Expr::Binary(
                SN![ast::BinOp::Add, 2, 3],
                SN![ast::Expr::Lit(SN![Lit![1 int], 0, 1]), 0, 1],
                SN![
                    ast::Expr::Call(
                        SN![
                            ast::Expr::Path(SN![
                                ast::Path {
                                    segments: vec![SN![Id![f], 4, 5]]
                                },
                                4,
                                5
                            ]),
                            4,
                            5
                        ],
                        vec![SN![
                            ast::Expr::Unary(
                                SN![ast::UnOp::Deref, 6, 7],
                                SN![
                                    ast::Expr::Addr(SN![
                                        ast::Expr::Lit(SN![Lit![2 int], 8, 9]),
                                        8,
                                        9
                                    ]),
                                    7,
                                    9
                                ]
                            ),
                            6,
                            9
                        ]]
                    ),
                    4,
                    10
                ]
            )
        );
    }

    #[test]
    fn parse_item() {
        let unit_struct = ast::Stmt::Item(SN![
            ast::Item {
                ident: SN![Id![Unit], 32, 36],
                kind: SN![
                    ast::ItemKind::Struct(ast::Struct {
                        generics: None,
                        fields: None
                    }),
                    25,
                    37
                ]
            },
            25,
            37
        ]);

        let drop_generics = Some(SN![
            ast::Generics {
                params: vec![SN![Id![T], 53, 54]]
            },
            52,
            55
        ]);
        let drop_fn_sig = ast::FnSig {
            inputs: SN![
                vec![SN![
                    ast::Param {
                        ident: SN![Id!["_"], 56, 57],
                        ty: SN![
                            ast::Ty::Path(SN![
                                ast::Path {
                                    segments: vec![SN![Id![T], 59, 60]]
                                },
                                59,
                                60
                            ]),
                            59,
                            60
                        ]
                    },
                    56,
                    60
                ]],
                55,
                61
            ],
            output: SN![ast::FnRetTy::Default(SN![(), 62, 61]), 62, 61],
        };
        let drop_fn = ast::Stmt::Item(SN![
            ast::Item {
                ident: SN![Id![drop], 48, 52],
                kind: SN![
                    ast::ItemKind::Func(ast::Func {
                        generics: drop_generics,
                        sig: drop_fn_sig,
                        block: SN![ast::Block { stmts: vec![] }, 62, 64]
                    }),
                    43,
                    64
                ]
            },
            43,
            64
        ]);

        let slice_fields = Some(SN![
            vec![
                SN![
                    ast::FieldDef {
                        ident: SN![Id![ptr], 100, 103],
                        ty: SN![
                            ast::Ty::Ptr(SN![
                                ast::Ty::Path(SN![
                                    ast::Path {
                                        segments: vec![SN![Id![T], 106, 107]]
                                    },
                                    106,
                                    107
                                ]),
                                105,
                                107
                            ]),
                            105,
                            107
                        ]
                    },
                    100,
                    107
                ],
                SN![
                    ast::FieldDef {
                        ident: SN![Id![len], 116, 119],
                        ty: SN![
                            ast::Ty::Path(SN![
                                ast::Path {
                                    segments: vec![SN![Id![USize], 121, 126]]
                                },
                                121,
                                126
                            ]),
                            121,
                            126
                        ]
                    },
                    116,
                    126
                ]
            ],
            90,
            132
        ]);
        let slice_struct = ast::Stmt::Item(SN![
            ast::Item {
                ident: SN![Id![Slice], 77, 82],
                kind: SN![
                    ast::ItemKind::Struct(ast::Struct {
                        generics: Some(SN![
                            ast::Generics {
                                params: vec![SN![Id![T], 83, 84]]
                            },
                            82,
                            85
                        ]),
                        fields: slice_fields
                    }),
                    70,
                    132
                ]
            },
            70,
            132
        ]);

        let local_init = ast::Stmt::Local(SN![
            ast::Local {
                ident: SN![Id![a], 142, 143],
                kind: SN![
                    ast::LocalKind::Init(
                        SN![ast::Expr::Lit(SN![Lit![1 int], 147, 148]), 147, 148],
                        None
                    ),
                    142,
                    148
                ]
            },
            142,
            148
        ]);
        let local_init_ty = ast::Stmt::Local(SN![
            ast::Local {
                ident: SN![Id![a], 154, 155],
                kind: SN![
                    ast::LocalKind::Init(
                        SN![ast::Expr::Lit(SN![Lit![1 int], 163, 164]), 163, 164],
                        Some(SN![
                            ast::Ty::Path(SN![
                                ast::Path {
                                    segments: vec![SN![Id![Int], 157, 160]]
                                },
                                157,
                                160
                            ]),
                            157,
                            160
                        ])
                    ),
                    154,
                    164
                ]
            },
            154,
            164
        ]);
        let local_decl = ast::Stmt::Local(SN![
            ast::Local {
                ident: SN![Id![a], 170, 171],
                kind: SN![
                    ast::LocalKind::Decl(SN![
                        ast::Ty::Path(SN![
                            ast::Path {
                                segments: vec![SN![Id![Int], 173, 176]]
                            },
                            173,
                            176
                        ]),
                        173,
                        176
                    ]),
                    170,
                    176
                ]
            },
            170,
            176
        ]);

        let expr = ast::Stmt::Expr(SN![ast::Expr::Lit(SN![Lit![0 int], 183, 184]), 183, 184]);

        expect_parse!(
            "func main() -> Int
{
    struct Unit;

    func drop<T>(_: T) {}

    struct Slice<T>
    {
        ptr: *T
        len: USize
    }
    
    a := 1;
    a: Int = 1;
    a: Int;

    0;
}",
            item_parser,
            ast::Item {
                ident: SN![ast::Ident::new("main"), 5, 9],
                kind: SN![
                    ast::ItemKind::Func(ast::Func {
                        generics: None,
                        sig: ast::FnSig {
                            inputs: SN![vec![], 9, 11],
                            output: SN![
                                ast::FnRetTy::Ty(SN![
                                    ast::Ty::Path(SN![
                                        ast::Path {
                                            segments: vec![SN![ast::Ident::new("Int"), 15, 18]]
                                        },
                                        15,
                                        18
                                    ]),
                                    12,
                                    18
                                ]),
                                12,
                                18
                            ]
                        },
                        block: SN![
                            ast::Block {
                                stmts: vec![
                                    unit_struct,
                                    drop_fn,
                                    slice_struct,
                                    local_init,
                                    local_init_ty,
                                    local_decl,
                                    expr
                                ]
                            },
                            19,
                            187
                        ]
                    }),
                    0,
                    187
                ]
            }
        );
    }
}
