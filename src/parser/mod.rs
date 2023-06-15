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
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{IResult, InputTakeAtPosition};
use std::collections::HashMap;

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

pub fn parse_items(input: &str) -> IResult<&str, Vec<Item>> {
    many0(terminated(parse_item, many0(alt((space0, line_ending)))))(input)
}

pub fn parse_item(input: &str) -> IResult<&str, Item> {
    alt((
        map(parse_import, |i| Item::Import(i)),
        map(parse_function_declare, |i| Item::FunctionDeclare(i)),
    ))(input)
}

pub fn parse_function_declare(input: &str) -> IResult<&str, FunctionDeclare> {
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
        ident,
        parameters,
        ret_type,
        block,
    };
    Ok((r, declare))
}

pub fn parse_import(input: &str) -> IResult<&str, Import> {
    let (res, key) = delimited(
        helpers::tailing_space_1(tag("use")),
        alpha1,
        helpers::leading_space_0(tag(";")),
    )(input)?;
    Ok((
        res,
        Import {
            name: key.to_owned(),
        },
    ))
}

/// TYPE = POINTER | REFERENCE | PLAIN_TYPE
pub fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((
        parse_pointer,
        parse_reference,
        map(tag("()"), |_| Type::Unit),
        map(parse_plain_type, |ty| Type::Plain(ty)),
    ))(input)
}

/// REFERENCE = & ~ TYPE
pub fn parse_reference(input: &str) -> IResult<&str, Type> {
    map(preceded(tag("&"), parse_type), |ty| {
        Type::Reference(Box::new(ty))
    })(input)
}

/// POINTER = * ~ TYPE
pub fn parse_pointer(input: &str) -> IResult<&str, Type> {
    map(preceded(tag("*"), parse_type), |ty| {
        Type::Pointer(Box::new(ty))
    })(input)
}

/// PLAIN_TYPE = TYPE_IDENTIFIER
pub fn parse_plain_type(input: &str) -> IResult<&str, PlainType> {
    map(parse_type_identifier, |s: &str| PlainType {
        name: s.to_owned(),
    })(input)
}

/// TYPE_IDENTIFIER = alpha ~ ("_" | alphanumber)*
pub fn parse_type_identifier(input: &str) -> IResult<&str, &str> {
    recognize(tuple((alpha1, many0(alt((tag("_"), alphanumeric1))))))(input)
}

/// IDENTIFIER = "_"? ~ ("_" | alphanumber)*
pub fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(many1(alt((tag("_"), alphanumeric1))))(input)
}

/// FUNCTION_PARAMETER = IDENTIFIER ~ ":" ~ TYPE
pub fn parse_function_parameter(input: &str) -> IResult<&str, (&str, Type)> {
    let (i, (ident, _, ty)) = tuple((
        parse_identifier,
        leading_space_0(tailing_space_0(tag(":"))),
        parse_type,
    ))(input)?;
    Ok((i, (ident, ty)))
}

pub fn parse_function_parameters(input: &str) -> IResult<&str, Vec<(&str, Type)>> {
    tailing_separator_list_0(",", parse_function_parameter)(input)
}

pub fn parse_statement(input: &str) -> IResult<&str, Statement> {
    map(terminated(parse_expression, tag(";")), |expr| {
        Statement::Expr(expr)
    })(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(parse_block, |block| Expression::Block(Box::new(block))),
        map(parse_identifier, |ident| Expression::Identifier(ident)),
        // map(parse_field_access, |(lhs, rhs)| {
        //     Expression::FieldAccess(Box::new(lhs), Box::new(rhs))
        // }),
    ))(input)
}

pub fn parse_field_access(input: &str) -> IResult<&str, (Expression, Expression)> {
    let (r, (lhs, _, rhs)) = tuple((parse_expression, tag("."), parse_expression))(input)?;
    Ok((r, (lhs, rhs)))
}

pub fn parse_block(input: &str) -> IResult<&str, Block> {
    let (r, (_, statements, expr, _)) = tuple((
        tailing_space_0(tag("{")),
        many0(parse_statement),
        opt(parse_expression),
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
            assert_eq!("", res);

            )+
        };
    }

    macro_rules! assert_parse {
        ($expected: expr, $parser:expr) => {
            let (res, output) = $parser.unwrap();
            assert_eq!("", res);
            assert_eq!($expected, output);
        };
    }

    mod import {
        use crate::parser::parse_import;

        #[test]
        fn should_parse_import() {
            easy_parse! {
                parse_import("use fmt ;"),
                parse_import("use fmt;"),
            }
        }
    }

    mod item {
        use crate::parser::parse_item;

        #[test]
        fn should_parse_import_item() {
            easy_parse! {
                parse_item("use fmt ;"),
            }
        }
    }

    mod r#type {
        use crate::ast::{PlainType, Type};
        use crate::parser::{parse_plain_type, parse_type};

        #[test]
        fn should_parse_plain_type() {
            assert_parse! {Type::Plain(PlainType {name: "MyStruct".to_owned()}), parse_type("MyStruct")}
        }
        #[test]
        fn should_parse_pointer_type() {
            assert_parse! {Type::Pointer(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))), parse_type("*MyStruct")}
        }
        #[test]
        fn should_parse_reference_type() {
            assert_parse! {Type::Reference(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))), parse_type("&MyStruct")}
            assert_parse! {Type::Reference(Box::new(Type::Pointer(Box::new(Type::Plain(PlainType {name: "MyStruct".to_owned()}))))), parse_type("&*MyStruct")}
        }
        #[test]
        fn should_parse_unit_type() {
            assert_parse! {Type::Unit, parse_type("()")}
        }
    }
    mod identifier {
        use crate::parser::parse_identifier;

        #[test]
        fn should_parse_identifier() {
            assert_parse! {"_", parse_identifier("_")}
            assert_parse! {"_123", parse_identifier("_123")}
            assert_parse! {"a", parse_identifier("a")}
            assert_parse! {"_a", parse_identifier("_a")}
            assert_parse! {"_a123", parse_identifier("_a123")}
            assert_parse! {"a_bsdf", parse_identifier("a_bsdf")}
            assert_parse! {"KFCVWO50", parse_identifier("KFCVWO50")}
        }
        #[test]
        fn should_not_parse() {
            assert!(dbg!(parse_identifier("")).is_err());
        }
    }
    mod function {
        use crate::ast::{PlainType, Type};
        use crate::parser::{parse_function_parameter, parse_function_parameters};

        #[test]
        fn should_parse_function_parameter() {
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter("my:MyStruct")}
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter("my :MyStruct")}
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter("my : MyStruct")}
            assert_parse! {("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), parse_function_parameter("my: MyStruct")}
        }
        #[test]
        fn should_parse_function_parameters() {
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters("my:MyStruct")}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters("my:MyStruct,")}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), ("my2", Type::Plain(PlainType {name: "MyStruct2".to_owned()}))], parse_function_parameters("my:MyStruct,my2:MyStruct2")}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()})), ("my2", Type::Plain(PlainType {name: "MyStruct2".to_owned()}))], parse_function_parameters("my:MyStruct,my2:MyStruct2,")}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters("my:MyStruct ,")}
            assert_parse! {vec![("my", Type::Plain(PlainType {name: "MyStruct".to_owned()}))], parse_function_parameters("my:MyStruct , ")}
            assert_parse! {Vec::<(&str, Type)>::new(), parse_function_parameters("")}
        }
    }
    mod block {
        use crate::ast::Block;
        use crate::parser::parse_block;
        #[test]
        fn should_parse_block() {
            assert_parse! {Block{statements: vec![], expr:None}, parse_block("{}")}
            assert_parse! {Block{statements: vec![], expr:None}, parse_block("{ }")}
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

            assert_parse! {declare, parse_function_declare("fn main() -> i32 {}")}
        }
    }
    mod expression {
        use crate::ast::{Block, Expression};
        use crate::parser::{parse_expression, parse_field_access};

        #[test]
        fn should_parse_block() {
            assert_parse! {Expression::Block(Box::new(Block{ statements: vec![], expr: None })), parse_expression("{}")}
        }

        #[test]
        fn should_parse_identifier() {
            assert_parse! {Expression::Identifier("my_struct"), parse_expression("my_struct")}
        }
        #[test]
        fn should_parse_field_access() {
            assert_parse! {(Expression::Identifier("my_struct"), Expression::Identifier("a")), dbg!(parse_field_access("my_struct.a"))}
            assert_parse! {Expression::FieldAccess(Box::new(Expression::Identifier("my_struct")), Box::new(Expression::Identifier("a"))), dbg!(parse_expression("my_struct.a"))}
        }
    }
}