pub struct GustFile<'a> {
    statements: Vec<Item<'a>>,
}

pub enum Item<'a> {
    Import(Import),
    FunctionDeclare(FunctionDeclare<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    Assignment(Assignment),
    Expr(Expression),
}



#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub identifier: String,
    pub value: Expression,
}




/// ```gust
/// let a = 1;
/// let a;
/// let a: i32 = 1;
/// ```
#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub mutable: bool,
    pub identifier: String,
    /// type can be inferred by the value
    pub ttype: Option<Type>,

    /// value can be initialised later.
    pub value: Option<Expression>
}


/// ```gust
/// use fmt;
/// use fmt::println;
/// use fmt::{println, print};
/// ```
#[derive(Debug, PartialEq)]
pub struct Import {
    pub module: String,
    pub items: Vec<String>,
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
    pub block: Block,
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
pub enum Expression {
    String(String),
    Block(Box<Block>),
    Identifier(String),
    FieldAccess(Box<Expression>, Box<Expression>),
    Number(i32),
    Bool(bool),
    BinOperation(Opcode, Box<Expression>, Box<Expression>),
    If {
        condition: Box<Expression>,
        then_body: Box<Expression>,
        else_body: Box<Expression>,
    },
    FunctionCall(Box<Expression>, Vec<Box<Expression>>),
    Group(Box<Expression>),
    Negative(Box<i32>),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Reference(Box<Type>),
    Pointer(Box<Type>),
    Array(Box<Type>),
    Plain(PlainType),
    Unit,
    Bool,

    String,

    Isize,
    Int8,
    Int16,
    Int32,
    Int64,

    Usize,
    Unt8,
    Unt16,
    Unt32,
    Unt64,

    Float32,
    Float64,
}

#[derive(Debug, PartialEq)]
pub struct PlainType {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub expr: Option<Expression>,
}
