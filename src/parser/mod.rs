use log::error;
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{
    alpha1, alphanumeric1, char, digit1, line_ending, one_of, space1,
};
use nom::combinator::{cut, map, opt, recognize};
use nom::error::context;
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Parser};
use nom_locate::LocatedSpan;

use crate::ast::{
    Assignment, Block, Expression, FunctionDeclare, GustFile, Import, Item, LetStatement,
    PlainType, Statement, Type,
};
use crate::parser::helpers::{
    leading_space_0, surrounding_space_0, surrounding_space_1, tailing_separator_list_0,
    tailing_space_0, tailing_space_1,
};

pub type Span<'a> = LocatedSpan<&'a str>;

pub mod helpers;

pub fn parse_file(input: Span) -> Result<GustFile, ()> {
    let result = parse_items(input);
    match result {
        Ok((_, stats)) => {
            let file = GustFile { items: stats };
            Ok(file)
        }
        Err(e) => {
            error!("parse error: {}", e);
            Err(())
        }
    }
}

pub fn parse_items(input: Span) -> IResult<Span, Vec<Item>> {
    many0(terminated(
        parse_item,
        opt(many0(alt((space1, line_ending)))),
    ))(input)
}

pub fn parse_item(input: Span) -> IResult<Span, Item> {
    alt((
        map(parse_import, |i| Item::Import(i)),
        map(parse_function_declare, |i| Item::FunctionDeclare(i)),
    ))(input)
}

pub fn parse_function_declare(input: Span) -> IResult<Span, FunctionDeclare> {
    let (r, (_, ident, _, parameters, _, ret_type, block)) = tuple((
        tailing_space_1(tag("fn")),
        tailing_space_0(parse_type_identifier),
        tailing_space_0(tag("(")),
        tailing_space_0(parse_function_parameters),
        tailing_space_0(tag(")")),
        opt(preceded(
            surrounding_space_0(tag("->")),
            surrounding_space_0(parse_type),
        )),
        surrounding_space_0(parse_block),
    ))(input)?;
    let parameters = parameters
        .into_iter()
        .map(|it| (it.0.to_string(), it.1))
        .collect();
    let declare = FunctionDeclare {
        ident: ident.fragment().to_string(),
        parameters,
        ret_type: ret_type.unwrap_or_else(|| Type::Unit),
        block,
    };
    Ok((r, declare))
}

/// ```syntax
/// use fmt;
/// use fmt::function;
/// use fmt::{function, function};
/// ```
pub fn parse_import(input: Span) -> IResult<Span, Import> {
    let (res, (module, items)) = delimited(
        helpers::tailing_space_1(tag("use")),
        tuple((alpha1, opt(parse_import_items))),
        helpers::leading_space_0(tag(";")),
    )(input)?;
    Ok((
        res,
        Import {
            module: module.fragment().to_string(),
            items: items.unwrap_or_default(),
        },
    ))
}

/// ```syntax
/// ::function
/// ::{function, function}
/// ```
pub fn parse_import_items(input: Span) -> IResult<Span, Vec<String>> {
    let (res, key) = preceded(
        tailing_space_0(tag("::")),
        alt((
            map(alpha1, |it| vec![it]),
            delimited(
                tailing_space_0(tag("{")),
                tailing_separator_list_0(",", alpha1),
                tailing_space_0(tag("}")),
            ),
        )),
    )(input)?;
    Ok((
        res,
        key.into_iter()
            .map(|it| it.fragment().to_string())
            .collect(),
    ))
}

/// TYPE = POINTER | REFERENCE | PLAIN_TYPE | PRIMITIVE_TYPE
pub fn parse_type(input: Span) -> IResult<Span, Type> {
    alt((
        parse_pointer,
        parse_reference,
        map(tag("()"), |_| Type::Unit),
        map(tag("String"), |_| Type::String),
        map(tag("boolean"), |_| Type::Bool),
        map(tag("isize"), |_| Type::Isize),
        map(tag("i8"), |_| Type::Int8),
        map(tag("i16"), |_| Type::Int16),
        map(tag("i32"), |_| Type::Int32),
        map(tag("i64"), |_| Type::Int64),
        map(tag("usize"), |_| Type::Usize),
        map(tag("u8"), |_| Type::Unt8),
        map(tag("u16"), |_| Type::Unt16),
        map(tag("u32"), |_| Type::Unt32),
        map(tag("u64"), |_| Type::Unt64),
        map(tag("f32"), |_| Type::Float32),
        map(tag("f64"), |_| Type::Float64),
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

/// ATOM = IDENTIFIER | BOOL | NUMBER | GROUP | STRING
pub fn parse_atom(input: Span) -> IResult<Span, Expression> {
    alt((
        parse_bool,
        parse_number,
        parse_string,
        map(parse_identifier, |it| {
            Expression::Identifier(it.fragment().to_string())
        }),
        parse_group_expression,
    ))(input)
}

/// STRING
///
/// todo: unicode support
pub fn parse_string(input: Span) -> IResult<Span, Expression> {
    let (res, s) = context(
        "string",
        preceded(char('\"'), cut(terminated(parse_string_inner, char('\"')))),
    )
    .parse(input)?;
    let expr = Expression::String(s.fragment().to_string());
    Ok((res, expr))
}
fn parse_string_inner(input: Span) -> IResult<Span, Span> {
    escaped(alt((alphanumeric1, space1)), '\\', one_of("\"n\\"))(input)
}

/// BOOL = true | false
pub fn parse_bool(input: Span) -> IResult<Span, Expression> {
    alt((
        map(tag("true"), |_| Expression::Bool(true)),
        map(tag("false"), |_| Expression::Bool(false)),
    ))(input)
}

/// NUMBER = MINUS? ~ PRIMITIVE_NUMBER ~ POSTFIX?
/// todo: handle float and uninteger
pub fn parse_number(input: Span) -> IResult<Span, Expression> {
    map(recognize(tuple((opt(tag("-")), digit1))), |it: Span| {
        Expression::Number(it.fragment().to_string().parse::<i32>().unwrap())
    })(input)
}

/// IDENTIFIER = "_"? ~ ("_" | alphanumber)*
pub fn parse_identifier(input: Span) -> IResult<Span, Span> {
    recognize(many1(alt((tag("_"), alphanumeric1))))(input)
}

/// FUNCTION_PARAMETER = IDENTIFIER ~ ":" ~ TYPE
pub fn parse_function_parameter(input: Span) -> IResult<Span, (&str, Type)> {
    let (i, (ident, _, ty)) =
        tuple((parse_identifier, surrounding_space_0(tag(":")), parse_type))(input)?;
    Ok((i, (ident.fragment(), ty)))
}

pub fn parse_function_parameters(input: Span) -> IResult<Span, Vec<(&str, Type)>> {
    tailing_separator_list_0(",", parse_function_parameter)(input)
}

pub fn parse_statement(input: Span) -> IResult<Span, Statement> {
    alt((
        parse_statement_let,
        parse_statement_assignment,
        parse_statement_expr,
    ))(input)
}

/// LET_STATEMENT = let ~ mut? ~ identifier ~ ": ttype"? ~ (= value)? ~ ;
pub fn parse_statement_let(input: Span) -> IResult<Span, Statement> {
    let (res, (mutable, ident, ident_type, value)) = delimited(
        leading_space_0(tailing_space_1(tag("let"))),
        tuple((
            opt(surrounding_space_1(tag("mut"))),
            parse_identifier,
            opt(preceded(surrounding_space_0(tag(":")), parse_type)),
            opt(preceded(surrounding_space_0(tag("=")), parse_expression)),
        )),
        surrounding_space_0(tag(";")),
    )(input)?;

    let let_stats = Statement::LetStatement(LetStatement {
        mutable: mutable.is_some(),
        identifier: ident.fragment().to_string(),
        ttype: ident_type,
        value,
    });

    Ok((res, let_stats))
}

/// ASSIGNMENT_STATEMENT =  identifier ~ = ~ value ~ ;
pub fn parse_statement_assignment(input: Span) -> IResult<Span, Statement> {
    let (res, (ident, _, value, _)) = tuple((
        parse_identifier,
        surrounding_space_0(tag("=")),
        parse_expression,
        surrounding_space_0(tag(";")),
    ))(input)?;

    let assignment_stats = Statement::Assignment(Assignment {
        identifier: ident.fragment().to_string(),
        value,
    });

    Ok((res, assignment_stats))
}

pub fn parse_statement_expr(input: Span) -> IResult<Span, Statement> {
    map(terminated(parse_expression, tag(";")), |expr| {
        Statement::Expr(expr)
    })(input)
}

pub fn parse_identifier_or_function_call(input: Span) -> IResult<Span, Expression> {
    let (r, (ident, params)) = tuple((
        parse_atom,
        opt(tuple((
            leading_space_0(tailing_space_0(tag("("))),
            tailing_space_0(parse_function_call_parameters),
            leading_space_0(tailing_space_0(tag(")"))),
        ))),
    ))(input)?;
    if let Some((_, params, _)) = params {
        let params = params.into_iter().map(Box::new).collect();
        Ok((r, Expression::FunctionCall(Box::new(ident), params)))
    } else {
        Ok((r, ident))
    }
}

pub fn parse_function_call_parameters(input: Span) -> IResult<Span, Vec<Expression>> {
    tailing_separator_list_0(",", parse_expression)(input)
}

pub fn parse_expression(input: Span) -> IResult<Span, Expression> {
    alt((
        map(parse_block, |block| Expression::Block(Box::new(block))),
        parse_field_access,
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
    let (r, mut exprs) = separated_list1(tag("."), parse_identifier_or_function_call)(s)?;
    let expr = match exprs.len() {
        1 => exprs.pop().unwrap(),
        _ => {
            let option = exprs.into_iter().fold(None, |acc, item| {
                if let Some(prev) = acc {
                    match item {
                        Expression::FunctionCall(fc, params) => Some(Expression::FunctionCall(
                            Box::new(Expression::FieldAccess(Box::new(prev), fc)),
                            params,
                        )),
                        _ => Some(Expression::FieldAccess(Box::new(prev), Box::new(item))),
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

pub fn parse_block(input: Span) -> IResult<Span, Block> {
    let (r, (_, statements, expr, _)) = tuple((
        surrounding_space_0(tag("{")),
        many0(parse_statement),
        opt(parse_expression),
        surrounding_space_0(tag("}")),
    ))(input)?;

    let block = Block { statements, expr };
    Ok((r, block))
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
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

    mod import {
        use crate::ast::Import;
        use crate::parser::parse_import;

        #[test]
        fn import_whole_module() {
            let import = Import {
                module: "fmt".to_string(),
                items: vec![],
            };
            assert_parse! { import, parse_import,
                "use fmt;"
            }
        }

        #[test]
        fn import_items() {
            let import = Import {
                module: "fmt".to_string(),
                items: vec!["println".to_string()],
            };
            assert_parse! { import, parse_import,
                "use fmt::println;"
            }

            let import = Import {
                module: "fmt".to_string(),
                items: vec!["println".to_string(), "print".to_string()],
            };
            assert_parse! { import, parse_import,
                "use fmt::{println, print};"
            }
        }
    }

    mod statement {
        mod let_statement {
            use crate::ast::{Expression, LetStatement, Statement, Type};
            use crate::parser::parse_statement;

            #[test]
            fn let_with_value() {
                let stats = Statement::LetStatement(LetStatement {
                    mutable: false,
                    identifier: "a".to_string(),
                    ttype: None,
                    value: Some(Expression::Number(1)),
                });
                assert_parse! {stats, parse_statement, "let a = 1;"}
                assert_parse! {stats, parse_statement, "let a=1;"}
                assert_parse! {stats, parse_statement, "let a= 1;"}
                assert_parse! {stats, parse_statement, "let a =1;"}
            }

            #[test]
            fn let_without_value() {
                let stats = Statement::LetStatement(LetStatement {
                    mutable: false,
                    identifier: "a".to_string(),
                    ttype: None,
                    value: None,
                });
                assert_parse! {stats, parse_statement, "let a;"}
                assert_parse! {stats, parse_statement, "let a ;"}
            }

            #[test]
            fn let_with_ttype() {
                let stats = Statement::LetStatement(LetStatement {
                    mutable: false,
                    identifier: "a".to_string(),
                    ttype: Some(Type::Int32),
                    value: None,
                });
                assert_parse! {stats, parse_statement, "let a: i32;"}
                assert_parse! {stats, parse_statement, "let a : i32 ;"}
                assert_parse! {stats, parse_statement, "let a :i32 ;"}
            }
            #[test]
            fn let_with_ttype_and_value() {
                let stats = Statement::LetStatement(LetStatement {
                    mutable: false,
                    identifier: "a".to_string(),
                    ttype: Some(Type::Int32),
                    value: Some(Expression::Number(1)),
                });
                assert_parse! {stats, parse_statement, "let a:i32 = 1;"}
                assert_parse! {stats, parse_statement, "let a : i32=1;"}
                assert_parse! {stats, parse_statement, "let a: i32 = 1;"}
                assert_parse! {stats, parse_statement, "let a :i32 =1;"}
            }
        }

        mod assignment {
            use crate::ast::{Assignment, Expression, Statement};
            use crate::parser::parse_statement;

            #[test]
            fn assignment_statement() {
                let stats = Statement::Assignment(Assignment {
                    identifier: "a".to_string(),
                    value: Expression::Bool(true),
                });
                assert_parse! {stats, parse_statement, "a = true;"}

                let stats = Statement::Assignment(Assignment {
                    identifier: "a".to_string(),
                    value: Expression::Identifier("b".to_string()),
                });
                assert_parse! {stats, parse_statement, "a = b;"}
            }
        }
    }

    mod r#type {
        use crate::ast::{PlainType, Type};
        use crate::parser::parse_type;

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

        #[test]
        fn primitive_type() {
            assert_parse! {Type::Bool, parse_type, "boolean" };
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
        use crate::parser::{parse_function_parameter, parse_function_parameters};

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
        use crate::ast::{Block, Expression, FunctionDeclare, PlainType, Statement, Type};
        use crate::parser::parse_function_declare;
        use indoc::indoc;

        #[test]
        fn should_parse_empty_function_with_i32_return_type() {
            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![],
                ret_type: Type::Int32,
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main() -> i32 {}"}
        }

        #[test]
        fn should_parse_empty_function_with_params_and_return_type() {
            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![(
                    "my".to_string(),
                    Type::Plain(PlainType {
                        name: "MyStruct".to_owned(),
                    }),
                )],
                ret_type: Type::Int32,
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main(my: MyStruct) -> i32 {}"}

            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![
                    (
                        "my".to_string(),
                        Type::Plain(PlainType {
                            name: "MyStruct".to_owned(),
                        }),
                    ),
                    (
                        "my2".to_string(),
                        Type::Plain(PlainType {
                            name: "MyStruct".to_owned(),
                        }),
                    ),
                ],
                ret_type: Type::Int32,
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main(my: MyStruct, my2:MyStruct) -> i32 {}"}
        }

        #[test]
        fn should_parse_with_trailing_comma() {
            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![
                    (
                        "my".to_string(),
                        Type::Plain(PlainType {
                            name: "MyStruct".to_owned(),
                        }),
                    ),
                    (
                        "my2".to_string(),
                        Type::Plain(PlainType {
                            name: "MyStruct".to_owned(),
                        }),
                    ),
                ],
                ret_type: Type::Int32,
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main(my: MyStruct, my2:MyStruct,) -> i32 {}"}
        }
        #[test]
        fn function_declare_without_return_type() {
            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![],
                ret_type: Type::Unit,
                block: Block {
                    statements: vec![],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,"fn main() {}"}
        }

        #[test]
        fn multiline() {
            let declare = FunctionDeclare {
                ident: "main".to_string(),
                parameters: vec![],
                ret_type: Type::Unit,
                block: Block {
                    statements: vec![Statement::Expr(Expression::FunctionCall(
                        Box::new(Expression::FieldAccess(
                            Box::new(Expression::Identifier("fmt".to_string())),
                            Box::new(Expression::Identifier("printLn".to_string())),
                        )),
                        vec![Box::new(Expression::String("hello world".to_string()))],
                    ))],
                    expr: None,
                },
            };

            assert_parse! {declare, parse_function_declare,
                indoc! {r#"
                    fn main() {
                        fmt.printLn("hello world");
                    }
                "#}
            }
        }
    }

    mod expression {
        use crate::ast::{Block, Expression};
        use crate::parser::parse_expression;

        #[test]
        fn should_parse_block() {
            assert_parse! {Expression::Block(Box::new(Block{ statements: vec![], expr: None })), parse_expression,
                "{}"
            }
        }

        #[test]
        fn should_parse_identifier() {
            assert_parse! {Expression::Identifier("my_struct".to_string()), parse_expression,
                "my_struct"
            }
        }

        #[test]
        fn should_parse_field_access() {
            let expr = Expression::FieldAccess(
                Box::new(Expression::Identifier("my_struct".to_string())),
                Box::new(Expression::Identifier("a".to_string())),
            );
            assert_parse! {expr, parse_expression,
                "my_struct.a"
            }
            let expr = Expression::FieldAccess(
                Box::new(Expression::FieldAccess(
                    Box::new(Expression::Identifier("my_struct".to_string())),
                    Box::new(Expression::Identifier("a".to_string())),
                )),
                Box::new(Expression::Identifier("b".to_string())),
            );
            assert_parse! {expr, parse_expression,
                "my_struct.a.b"
            }
        }

        #[test]
        fn should_arse_group_expression() {
            let expr = Expression::Group(Box::new(Expression::Identifier("my_struct".to_string())));
            assert_parse! {expr, parse_expression,
                "(my_struct)"
            }
            let expr = Expression::FieldAccess(
                Box::new(Expression::Group(Box::new(Expression::FieldAccess(
                    Box::new(Expression::Identifier("my_struct".to_string())),
                    Box::new(Expression::Identifier("a".to_string())),
                )))),
                Box::new(Expression::Identifier("b".to_string())),
            );
            assert_parse! {expr, parse_expression,
                "(my_struct.a).b"
            }
        }

        #[test]
        fn should_parse_function_call() {
            let expr = Expression::FieldAccess(
                Box::new(Expression::FunctionCall(
                    Box::new(Expression::Identifier("a".to_string())),
                    vec![],
                )),
                Box::new(Expression::Identifier("b".to_string())),
            );
            assert_parse! { expr, parse_expression,
                "a().b"
            }

            let expr = Expression::FunctionCall(
                Box::new(Expression::Identifier("func".to_string())),
                vec![],
            );
            assert_parse! { expr, parse_expression,
                "func()"
            }

            let expr = Expression::FunctionCall(
                Box::new(Expression::FieldAccess(
                    Box::new(Expression::Identifier("fmt".to_string())),
                    Box::new(Expression::Identifier("printLn".to_string())),
                )),
                vec![],
            );
            assert_parse! {expr, parse_expression,
                "fmt.printLn()"
            }

            let expr = Expression::FunctionCall(
                Box::new(Expression::FieldAccess(
                    Box::new(Expression::FunctionCall(
                        Box::new(Expression::FieldAccess(
                            Box::new(Expression::Identifier("fmt".to_string())),
                            Box::new(Expression::Identifier("a".to_string())),
                        )),
                        vec![],
                    )),
                    Box::new(Expression::Identifier("b".to_string())),
                )),
                vec![],
            );
            assert_parse! { expr, parse_expression,
                "fmt.a().b()"
            }
        }

        #[test]
        fn should_parse_function_call_with_params() {
            let expr = Expression::FunctionCall(
                Box::new(Expression::Identifier("func".to_string())),
                vec![Box::new(Expression::Identifier("a".to_string()))],
            );

            assert_parse! { expr, parse_expression,
                "func(a)"
            }

            let expr = Expression::FunctionCall(
                Box::new(Expression::Identifier("func".to_string())),
                vec![
                    Box::new(Expression::Identifier("a".to_string())),
                    Box::new(Expression::Identifier("b".to_string())),
                ],
            );

            assert_parse! { expr, parse_expression,
                "func(a,b)"
            }

            assert_parse! { expr, parse_expression,
                "func(a,b,)"
            }
        }

        mod string {
            use crate::ast::Expression;
            use crate::parser::parse_expression;

            #[test]
            fn alphanumeric() {
                let expr = Expression::String("123".to_string());
                assert_parse! { expr, parse_expression, r#""123""#}

                let expr = Expression::String("a123".to_string());
                assert_parse! { expr, parse_expression, r#""a123""#}
            }
            #[test]
            fn with_space() {
                let expr = Expression::String("hello world".to_string());
                assert_parse! { expr, parse_expression, r#""hello world""#}
            }
        }
    }

    mod number {
        use crate::ast::Expression;
        use crate::parser::parse_number;

        #[test]
        fn primitive_number() {
            assert_parse! {Expression::Number(1), parse_number, "1"}
        }
        #[test]
        fn negative_number() {
            assert_parse! {Expression::Number(-1), parse_number, "-1"}
        }
    }
}
