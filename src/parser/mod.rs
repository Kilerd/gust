use crate::ast::{
    Block, Expression, FunctionDeclare, GustFile, Import, Item, PlainType, Statement, Type,
};
use crate::parser::helpers::{
    leading_space_0, tailing_separator_list_0, tailing_space_0, tailing_space_1,
};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{
    alpha0, alpha1, alphanumeric0, alphanumeric1, line_ending, newline, space0, space1,
};
use nom::character::{is_newline, is_space};
use nom::combinator::{cond, map, map_res, opt, recognize, value};
use nom::multi::{fold_many1, many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{IResult, InputTakeAtPosition};
use std::collections::HashMap;
use std::process::id;

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

mod helpers;
//
// pub fn parse_file(input: &str) -> IResult<&str, GustFile> {
//     // let result = many0(
//     //     alt((
//     //         parse_statement,
//     //         parse_import
//     //         ))
//     // )(input)?;
//     todo!()
// }
//

pub fn parse_items(input: Span) -> IResult<Span, Vec<Item>> {
    many0(terminated(parse_item, many0(alt((space0, line_ending)))))(input)
}

pub fn parse_item(input: Span) -> IResult<Span, Item> {
    alt((
        map(parse_import, |i| Item::Import(i)),
        map(parse_function_declare, |i| Item::FunctionDeclare(i)),
    ))(input)
}

pub fn parse_function_declare(input: Span) -> IResult<Span, FunctionDeclare> {
    let (r, (_, ident, _, parameters, _, _, ret_type, block)) = tuple((
        tailing_space_1(tag("fn")),
        tailing_space_0(parse_type_identifier),
        tailing_space_0(tag("(")),
        tailing_space_0(parse_function_parameters),
        tailing_space_0(tag(")")),
        tailing_space_0(tag("->")),
        tailing_space_0(parse_type),
        tailing_space_0(parse_block),
    ))(input)?;
    let declare = FunctionDeclare {
        ident: ident.fragment(),
        parameters,
        ret_type,
        block,
    };
    Ok((r, declare))
}

pub fn parse_import(input: Span) -> IResult<Span, Import> {
    let (res, key) = delimited(
        helpers::tailing_space_1(tag("use")),
        alpha1,
        helpers::leading_space_0(tag(";")),
    )(input)?;
    Ok((
        res,
        Import {
            name: key.fragment().to_string(),
        },
    ))
}

/// TYPE = POINTER | REFERENCE | PLAIN_TYPE
pub fn parse_type(input: Span) -> IResult<Span, Type> {
    alt((
        parse_pointer,
        parse_reference,
        map(tag("()"), |_| Type::Unit),
        map(parse_plain_type, |ty| Type::Plain(ty)),
    ))(input)
}

/// REFERENCE = & ~ TYPE
pub fn parse_reference(input: Span) -> IResult<Span, Type> {
    map(preceded(tag("&"), parse_type), |ty| {
        Type::Reference(Box::new(ty))
    })(input)
}

/// POINTER = * ~ TYPE
pub fn parse_pointer(input: Span) -> IResult<Span, Type> {
    map(preceded(tag("*"), parse_type), |ty| {
        Type::Pointer(Box::new(ty))
    })(input)
}

/// PLAIN_TYPE = TYPE_IDENTIFIER
pub fn parse_plain_type(input: Span) -> IResult<Span, PlainType> {
    map(parse_type_identifier, |s| PlainType {
        name: s.fragment().to_string(),
    })(input)
}

/// TYPE_IDENTIFIER = alpha ~ ("_" | alphanumber)*
pub fn parse_type_identifier(input: Span) -> IResult<Span, Span> {
    recognize(tuple((alpha1, many0(alt((tag("_"), alphanumeric1))))))(input)
}

/// IDENTIFIER = "_"? ~ ("_" | alphanumber)*
pub fn parse_identifier(input: Span) -> IResult<Span, Span> {
    recognize(many1(alt((tag("_"), alphanumeric1))))(input)
}

/// FUNCTION_PARAMETER = IDENTIFIER ~ ":" ~ TYPE
pub fn parse_function_parameter(input: Span) -> IResult<Span, (&str, Type)> {
    let (i, (ident, _, ty)) = tuple((
        parse_identifier,
        leading_space_0(tailing_space_0(tag(":"))),
        parse_type,
    ))(input)?;
    Ok((i, (ident.fragment(), ty)))
}

pub fn parse_function_parameters(input: Span) -> IResult<Span, Vec<(&str, Type)>> {
    tailing_separator_list_0(",", parse_function_parameter)(input)
}

pub fn parse_statement(input: Span) -> IResult<Span, Statement> {
    map(terminated(parse_single_expression, tag(";")), |expr| {
        Statement::Expr(expr)
    })(input)
}

pub fn parse_identifier_or_function_call(input: Span) -> IResult<Span, Expression> {
    let (r, (ident, params)) = tuple((
        parse_identifier,
        opt(tuple((
            leading_space_0(tailing_space_0(tag("("))),
            leading_space_0(tailing_space_0(tag(")"))),
        ))),
    ))(input)?;
    if let Some(params) = params {
        Ok((
            r,
            Expression::FunctionCall(Box::new(Expression::Identifier(ident.fragment())), vec![]),
        ))
    } else {
        Ok((r, Expression::Identifier(ident.fragment())))
    }
}

pub fn parse_single_expression(input: Span) -> IResult<Span, Expression> {
    alt((
        map(parse_block, |block| Expression::Block(Box::new(block))),
        parse_identifier_or_function_call,
        parse_group_expression,
    ))(input)
}

pub fn parse_group_expression(input: Span) -> IResult<Span, Expression> {
    let (r, (_, expr, _)) = tuple((
        leading_space_0(tailing_space_0(tag("("))),
        parse_expression,
        leading_space_0(tailing_space_0(tag(")"))),
    ))(input)?;
    let expr = Expression::Group(Box::new(expr));
    Ok((r, expr))
}
pub fn parse_field_access(s: Span) -> IResult<Span, Expression> {
    let (r, mut exprs) = separated_list1(tag("."), parse_single_expression)(s)?;
    let expr = match exprs.len() {
        1 => exprs.pop().unwrap(),
        _ => {
            let option = exprs.into_iter().fold(None, |acc, item| {
                if let Some(prev) = acc {

                    match item {
                        Expression::FunctionCall(fc, params) => {
                            Some(Expression::FunctionCall(
                                Box::new(Expression::FieldAccess(Box::new(prev), fc))
                                ,params
                            ))
                        }
                        _ => { Some(Expression::FieldAccess(Box::new(prev), Box::new(item)))}
                    }

                } else {
                    Some(item)
                }
            });
            option.unwrap()
        }
    };
    Ok((r, expr))
}

pub fn parse_expression(s: Span) -> IResult<Span, Expression> {
    parse_field_access(s)
}

pub fn parse_block(input: Span) -> IResult<Span, Block> {
    let (r, (_, statements, expr, _)) = tuple((
        tailing_space_0(tag("{")),
        many0(parse_statement),
        opt(parse_single_expression),
        tailing_space_0(tag("}")),
    ))(input)?;

    let block = Block { statements, expr };
    Ok((r, block))
}

#[cfg(test)]
mod test {
    use std::error::Error;

    macro_rules! easy_parse {
        ($($parse_stat: expr ,)+) => {

            $(
            let (res, _) = $parse_stat.unwrap();
            assert_eq!(&"", res.fragment());

            )+
        };
    }

    macro_rules! assert_parse {
        ($expected: expr, $parser_name:ident,  $parse_content:expr) => {
            let (res, output) = $parser_name(crate::parser::Span::new($parse_content)).unwrap();
            assert_eq!(&"", res.fragment(), "content leave should be empty");
            assert_eq!($expected, output);
        };
    }
    macro_rules! assert_parse_fragment {
        ($expected: expr, $parser_name:ident,  $parse_content:expr) => {
            let (res, output) = $parser_name(crate::parser::Span::new($parse_content)).unwrap();
            assert_eq!(&"", res.fragment(), "content leave should be empty");
            assert_eq!(&$expected, output.fragment());
        };
    }

    mod r#type {
        use crate::ast::{PlainType, Type};
        use crate::parser::{parse_plain_type, parse_type, Span};

        #[test]
        fn should_parse_plain_type() {
            assert_parse! {Type::Plain(PlainType {name: "MyStruct".to_owned()}), parse_type,
                "MyStruct"
            }
        }

        #[test]
        fn should_parse_pointer_type() {
            assert_parse! {Type::Pointer(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))), parse_type,
                "*MyStruct"
            }
        }

        #[test]
        fn should_parse_reference_type() {
            assert_parse! {Type::Reference(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))), parse_type,
                "&MyStruct"
            }
            assert_parse! {Type::Reference(Box::new(Type::Pointer(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))))), parse_type,"&*MyStruct"}
        }

        #[test]
        fn should_parse_unit_type() {
            assert_parse! {Type::Unit, parse_type,
                "()"
            }
        }
    }

    mod identifier {
        use crate::parser::{parse_identifier, Span};

        #[test]
        fn should_parse_identifier() {
            assert_parse_fragment! {"_", parse_identifier,
                "_"
            }
            assert_parse_fragment! {"_123", parse_identifier,
                "_123"
            }
            assert_parse_fragment! {"a", parse_identifier,
                "a"
            }
            assert_parse_fragment! {"_a", parse_identifier,
                "_a"
            }
            assert_parse_fragment! {"_a123", parse_identifier,
                "_a123"
            }
            assert_parse_fragment! {"a_bsdf", parse_identifier,
                "a_bsdf"
            }
            assert_parse_fragment! {"KFCVWO50", parse_identifier,
                "KFCVWO50"
            }
        }

        #[test]
        fn should_not_parse() {
            assert!(dbg!(parse_identifier(Span::new(""))).is_err());
        }
    }

    mod function {
        use crate::ast::{PlainType, Type};
        use crate::parser::{parse_function_parameter, parse_function_parameters, Span};

        #[test]
        fn should_parse_function_parameter() {
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter,
                "my:MyStruct"
            }
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter,
                "my :MyStruct"
            }
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter,
                "my : MyStruct"
            }
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter,
                "my: MyStruct"
            }
        }

        #[test]
        fn should_parse_function_parameters() {
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters,"my:MyStruct"}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters,"my:MyStruct,"}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), ("my2", Type::Plain(PlainType {name: "MyStruct2".to_owned()}))], parse_function_parameters,"my:MyStruct,my2:MyStruct2"}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), ("my2", Type::Plain(PlainType {name: "MyStruct2".to_owned()}))], parse_function_parameters,"my:MyStruct,my2:MyStruct2,"}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters,"my:MyStruct ,"}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters,"my:MyStruct , "}
            assert_parse! {Vec::<(&str, Type)>::new(), parse_function_parameters, ""}
        }
    }

    mod block {
        use crate::ast::Block;
        use crate::parser::parse_block;

        #[test]
        fn should_parse_block() {
            assert_parse! {Block{statements: vec![], expr:None}, parse_block, "{}"}
            assert_parse! {Block{statements: vec![], expr:None}, parse_block, "{ }"}
        }
    }

    mod function_declare {
        use crate::ast::{Block, FunctionDeclare, PlainType, Type};
        use crate::parser::parse_function_declare;

        #[test]
        fn should_parse_empty_function_with_i32_return_type() {
            let declare = FunctionDeclare {
                ident: "main",
                parameters: vec![],
                ret_type: Type::Plain(PlainType {
                    name: "i32".to_owned(),
                }),
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main() -> i32 {}"}
        }
    }

    mod expression {
        use crate::ast::{Block, Expression};
        use crate::parser::{parse_expression, parse_single_expression};

        #[test]
        fn should_parse_block() {
            assert_parse! {Expression::Block(Box::new(Block{ statements: vec![], expr: None })), parse_expression,
                "{}"
            }
        }

        #[test]
        fn should_parse_identifier() {
            assert_parse! {Expression::Identifier("my_struct"), parse_expression,
                "my_struct"
            }
        }

        #[test]
        fn should_parse_field_access() {
            assert_parse! {Expression::FieldAccess(Box::new(Expression::Identifier("my_struct")), Box::new(Expression::Identifier("a"))), parse_expression,
                "my_struct.a"
            }
            assert_parse! {Expression::FieldAccess(Box::new(Expression::FieldAccess(Box::new(Expression::Identifier("my_struct")), Box::new(Expression::Identifier("a")))), Box::new(Expression::Identifier("b"))), parse_expression,
                "my_struct.a.b"
            }
        }

        #[test]
        fn should_arse_group_expression() {
            assert_parse! {Expression::Group(Box::new(Expression::Identifier("my_struct"))), parse_expression,
                "(my_struct)"
            }
            assert_parse! {Expression::FieldAccess(Box::new(Expression::Group(Box::new(Expression::FieldAccess(Box::new(Expression::Identifier("my_struct")), Box::new(Expression::Identifier("a")))))), Box::new(Expression::Identifier("b"))), parse_expression,
                "(my_struct.a).b"
            }
        }

        #[test]
        fn should_parse_function_call() {
            assert_parse! { Expression::FieldAccess(Box::new(Expression::FunctionCall(Box::new(Expression::Identifier("a")), vec![])), Box::new(Expression::Identifier("b"))), parse_expression,
                "a().b"
            }
            assert_parse! { Expression::FunctionCall(Box::new(Expression::Identifier("func")), vec![]), parse_expression,
                "func()"
            }
            assert_parse! { Expression::FunctionCall(
                Box::new(Expression::FieldAccess(Box::new(Expression::Identifier("fmt")), Box::new(Expression::Identifier("printLn")))), vec![]
            ), parse_expression,
                "fmt.printLn()"
            }

            assert_parse! {
                Expression::FunctionCall(
                    Box::new(Expression::FieldAccess(
                        Box::new(Expression::FunctionCall(
                            Box::new(Expression::FieldAccess(Box::new(Expression::Identifier("fmt")), Box::new(Expression::Identifier("a")))), vec![]
                        )),
                        Box::new(Expression::Identifier("b"))
                    )),
                    vec![]
                )
                , parse_expression,
                "fmt.a().b()"
            }
        }
    }
}
