use crate::parser::{ExprTree, Instruction, Program, Statement, Type, Variable};
use crate::tokenizer::Token;
use std::collections::HashMap;

pub enum InterVar {
    Integer(i64),
    Real(f64),
    Character(char),
    CharacterChain(Box<str>),
    Boolean(bool),
}

impl ToString for InterVar {
    fn to_string(&self) -> String {
        match self {
            Self::Integer(x) => x.to_string(),
            Self::Real(x) => x.to_string(),
            Self::Character(x) => String::from(*x),
            Self::CharacterChain(x) => x.clone().into_string(),
            Self::Boolean(x) => x.to_string(),
        }
    }
}

fn evaluate_expression(expr: ExprTree, variables: &mut HashMap<Box<str>, InterVar>) -> InterVar {
    match expr.token {
        _ => panic!()
    }
}

fn interpret_statement(statement: Statement, variables: &mut HashMap<Box<str>, InterVar>) {
    match statement {
        Statement::SingleInstruction(Instruction::Assign(ident, expr)) => {
            let result = evaluate_expression(expr, variables);
            variables.insert(ident, result);
        }
        Statement::SingleInstruction(Instruction::Write(tokens)) => {
            let mut buffer = String::new();
            for token in tokens {
                if let Token::StringLiteral(str, _) = token {
                    buffer.push_str(&str);
                } else if let Token::Identifier(ident) = token {
                    buffer.push_str(&variables.get(&ident).unwrap().to_string());
                } else {
                    buffer.push_str(&token.to_string());
                }
            }
            println!("{buffer}");
        }
        Statement::SingleInstruction(Instruction::Read(idents)) => {
            let mut buffer = String::new();
            for ident in idents {
                std::io::stdin().read_line(&mut buffer).unwrap();
                match &variables.get(&ident).unwrap() {
                    InterVar::Integer(_) => variables.insert(ident, InterVar::Integer(buffer.trim().parse().unwrap())),
                    InterVar::Real(_) => variables.insert(ident, InterVar::Real(buffer.trim().parse().unwrap())),
                    InterVar::Character(_) => variables.insert(ident, InterVar::Character(buffer.trim().parse().unwrap())),
                    InterVar::CharacterChain(_) => variables.insert(ident, InterVar::CharacterChain(Box::from(buffer.trim()))),
                    InterVar::Boolean(_) => variables.insert(ident, InterVar::Boolean(buffer.trim().parse().unwrap())),
                };
                buffer.clear();
            }
        }
        _ => todo!(),
    }
}

pub fn interpret(program: Program) {
    let mut variables: HashMap<Box<str>, InterVar> = HashMap::new();
    for (ident, var) in program.variables {
        variables.insert(
            ident,
            match var.var_type {
                Type::Integer => InterVar::Integer(Default::default()),
                Type::Real => InterVar::Real(Default::default()),
                Type::Character => InterVar::Character(Default::default()),
                Type::CharacterChain => InterVar::CharacterChain(Default::default()),
                Type::Boolean => InterVar::Boolean(Default::default()),
            },
        );
    }
    for statement in program.statements {
        interpret_statement(statement, &mut variables);
    }
}
