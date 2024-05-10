use itertools::Itertools;

use crate::ast::{
    Assignment, Block, Expression, FunctionDeclare, GustFile, Import, Item, LetStatement,
    Statement, Type,
};

pub trait Transform {
    fn to_go(&self) -> String;
}

impl Transform for GustFile {
    fn to_go(&self) -> String {
        self.items.iter().map(|it| it.to_go()).join("\n")
    }
}

impl Transform for Item {
    fn to_go(&self) -> String {
        match self {
            Item::Import(import) => import.to_go(),
            Item::FunctionDeclare(func_declare) => func_declare.to_go(),
        }
    }
}

impl Transform for Import {
    fn to_go(&self) -> String {
        format!("import \"{}\"", self.module)
    }
}

impl Transform for FunctionDeclare {
    fn to_go(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|it| format!("{} {}", it.0, it.1.to_go()))
            .join(", ");
        format!(
            "func {name}({params}) {ret} {block}",
            name = self.ident,
            ret = self.ret_type.to_go(),
            block = self.block.to_go()
        )
    }
}
impl Transform for Type {
    fn to_go(&self) -> String {
        match self {
            Type::Plain(it) => it.name.to_owned(),
            Type::Unit => "()".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "String".to_string(),
            _ => todo!(),
        }
    }
}

impl Transform for Block {
    fn to_go(&self) -> String {
        format!("{{ {}  {} }}", self.statements.to_go(), self.expr.to_go())
    }
}

impl Transform for Vec<Statement> {
    fn to_go(&self) -> String {
        self.iter().map(|it| it.to_go()).join("\n")
    }
}

impl Transform for Statement {
    fn to_go(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.to_go(),
            Statement::Assignment(assignment) => assignment.to_go(),
            Statement::Expr(expr) => {
                format!("{};", expr.to_go())
            }
        }
    }
}

impl Transform for Option<Expression> {
    fn to_go(&self) -> String {
        "".to_owned()
    }
}

impl Transform for LetStatement {
    fn to_go(&self) -> String {
        let mut builder = "".to_string();
        builder.push_str("let");
        if self.mutable {
            builder.push_str(" mut");
        }
        builder.push_str(&format!(" {}", self.identifier.as_str()));
        if let Some(ttype) = &self.ttype {
            builder.push_str(&format!(": {}", ttype.to_go()))
        };

        if let Some(value) = &self.value {
            builder.push_str(&format!("= {}", value.to_go()))
        };
        builder.push(';');

        builder
    }
}
impl Transform for Assignment {
    fn to_go(&self) -> String {
        format!(
            "{ident} = {value};",
            ident = self.identifier.as_str(),
            value = self.value.to_go()
        )
    }
}

impl Transform for Expression {
    fn to_go(&self) -> String {
        match self {
            Expression::String(s) => {
                format!("\"{}\"", s)
            }
            Expression::Block(block) => block.to_go(),
            Expression::Identifier(i) => i.to_owned(),
            Expression::FieldAccess(ident, field) => {
                format!("{}.{}", ident.to_go(), field.to_go())
            }
            Expression::Number(number) => number.to_string(),
            Expression::Bool(b) => {
                if *b {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Expression::BinOperation(_, _, _) => {
                todo!()
            }
            Expression::If { .. } => {
                todo!()
            }
            Expression::FunctionCall(func_name, params) => {
                let params_str = params.iter().map(|it| it.to_go()).join(",");

                format!("{}({})", func_name.to_go(), params_str)
            }
            Expression::Group(group) => {
                format!("({})", group.to_go())
            }
        }
    }
}
