use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space0, space1};
use nom::combinator::{map, opt};
use nom::error::ParseError;
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded, terminated};
use nom::{AsChar, Compare, IResult, InputIter, InputLength, InputTakeAtPosition, Parser, Slice};
use std::ops::RangeFrom;

use crate::parser::Span;

pub fn surrounding_space_0<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    delimited(
        many0(alt((space1, tag("\n"), tag("\r\n")))),
        f,
        many0(alt((space1, tag("\n"), tag("\r\n")))),
    )
}
pub fn surrounding_space_1<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    delimited(
        many1(alt((space1, tag("\n"), tag("\r\n")))),
        f,
        many1(alt((space1, tag("\n"), tag("\r\n")))),
    )
}

pub fn tailing_space_1<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    terminated(f, many1(alt((space1, tag("\n"), tag("\r\n")))))
}

pub fn tailing_space_0<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    terminated(f, many0(alt((space1, tag("\n"), tag("\r\n")))))
}

pub fn leading_space_1<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    preceded(many1(alt((space1, tag("\n"), tag("\r\n")))), f)
}

pub fn leading_space_0<'a, O: 'a, E, F, I>(f: F) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    I: InputTakeAtPosition
        + std::clone::Clone
        + nom::InputIter
        + nom::InputLength
        + Compare<&'a str>
        + nom::InputTake
        + 'a,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E> + 'a,
    E: ParseError<I> + 'a,
{
    preceded(many0(alt((space1, tag("\n"), tag("\r\n")))), f)
}

pub fn tailing_separator_list_0<'a, O, E, F>(
    sep: &'a str,
    mut f: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Vec<O>, E>
where
    F: Parser<Span<'a>, O, E>,
    E: ParseError<Span<'a>> + 'a,
{
    map(opt(tailing_separator_list_1(sep, f)), |o| {
        o.unwrap_or_default()
    })
}
pub fn tailing_separator_list_1<'a, O, E, F>(
    sep: &'a str,
    mut f: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Vec<O>, E>
where
    F: Parser<Span<'a>, O, E>,
    E: ParseError<Span<'a>> + 'a,
{
    terminated(
        separated_list1(leading_space_0(tailing_space_0(tag(sep.clone()))), f),
        opt(leading_space_0(tailing_space_0(tag(sep)))),
    )
}
