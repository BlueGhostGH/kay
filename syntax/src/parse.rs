use chumsky::{
    primitive::{choice, end, just},
    recovery::nested_delimiters,
    recursive::recursive,
    select, Parser,
};

use crate::{
    ast,
    error::{Error, Pattern},
    node::SrcNode,
    span::Span,
    token::{Delimiter, Token},
};

pub mod helper {
    use crate::{error, token::Token};

    pub trait Parser<T> = chumsky::Parser<Token, T, Error = error::Error> + Clone;

    pub type BoxedParser<'a, T> = chumsky::BoxedParser<'a, Token, T, error::Error>;
}

use helper::BoxedParser;

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

pub fn nested_parser<'a, T, P, F>(parser: P, delimiter: Delimiter, f: F) -> BoxedParser<'a, T>
where
    T: 'a,
    P: helper::Parser<T> + 'a,
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
                let kind = ast::ItemKind::Struct { generics, fields };

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
            .ignore_then(ty_parser().map_with_span(SrcNode::new))
            .map(ast::FnRetTy::Ty)
            .or(just(Token::Semicolon)
                .map_with_span(|_, span| ast::FnRetTy::Default(SrcNode::new((), span))))
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
                let kind = ast::ItemKind::Func {
                    generics,
                    sig,
                    block,
                };

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
