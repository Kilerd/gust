use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, opt};
use nom::error::ParseError;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{preceded, terminated};
use nom::{IResult, InputLength, InputTakeAtPosition, Parser};

pub fn tailing_space_1<I, O1, E: nom::error::ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: nom::AsChar + Clone,
    F: nom::Parser<I, O1, E>,
{
    terminated(f, space1)
}

pub fn tailing_space_0<I, O1, E: nom::error::ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: nom::AsChar + Clone,
    F: nom::Parser<I, O1, E>,
{
    terminated(f, space0)
}

pub fn leading_space_1<I, O1, E: nom::error::ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: nom::AsChar + Clone,
    F: nom::Parser<I, O1, E>,
{
    preceded(space1, f)
}

pub fn leading_space_0<I, O1, E: nom::error::ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: nom::AsChar + Clone,
    F: nom::Parser<I, O1, E>,
{
    preceded(space0, f)
}

pub fn tailing_separator_list_0< 'a, O, E, F, >(
    sep: &'a str,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    F: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    map(opt(tailing_separator_list_1(sep, f)), |o|o.unwrap_or_default())
}
pub fn tailing_separator_list_1< 'a, O, E, F, >(
    sep: &'a str,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
    where
        F: Parser<&'a str, O, E>,
        E: ParseError<&'a str>,
{
    terminated(
        separated_list1(leading_space_0(tailing_space_0(tag(sep.clone()))), f),
        opt(leading_space_0(tailing_space_0(tag(sep)))),
    )
}
