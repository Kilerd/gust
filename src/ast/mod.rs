pub struct GustFile<'a> {
    statements: Vec<Item<'a>>,
}

pub enum Item<'a> {
    Import(Import),
    FunctionDeclare(FunctionDeclare<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expr(Expression<'a>),
}

/// ```gust
/// use fmt;
/// use fmt::println;
/// use fmt::{println, print};
/// ```
pub struct Import {
    pub name: String,
}

pub type Parameter<'a> = (&'a str, Type);

/// ```gust
/// pub fn main() {
///    println("Hello, 世界");
/// }
/// ```
#[derive(Debug, PartialEq)]
pub struct FunctionDeclare<'a> {
    pub ident: &'a str,
    pub parameters: Vec<Parameter<'a>>,
    pub ret_type: Type,
    pub block: Block<'a>,
}

#[derive(Debug, PartialOrd, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,

    Mod,
    Pow,

    Or,
    And,
    Xor,

    LShift,
    RShift,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    String(String),
    Block(Box<Block<'a>>),
    Identifier(&'a str),
    FieldAccess(Box<Expression<'a>>, Box<Expression<'a>>),
    Number(i32),
    BinOperation(Opcode, Box<Expression<'a>>, Box<Expression<'a>>),
    If {
        condition: Box<Expression<'a>>,
        then_body: Box<Expression<'a>>,
        else_body: Box<Expression<'a>>,
    },
    FunctionCall(Box<Expression<'a>>, Vec<Box<Expression<'a>>>),
    Group(Box<Expression<'a>>),
    Negative(Box<i32>),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Reference(Box<Type>),
    Pointer(Box<Type>),
    Array(Box<Type>),
    Plain(PlainType),
    Unit,
}

#[derive(Debug, PartialEq)]
pub struct PlainType {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub expr: Option<Expression<'a>>,
}
