use crate::parser::{ExprTree, Instruction, Program, Statement, Type};
use crate::tokenizer::Token;
use std::{
    collections::HashMap, ops::Add, ops::BitAnd, ops::BitOr, ops::BitXor, ops::Div, ops::Mul,
    ops::Not, ops::Sub,
};

#[derive(Clone, PartialEq, PartialOrd)]
enum InterValue {
    Integer(i64),
    Real(f64),
    Character(char),
    CharacterChain(Box<str>),
    Boolean(bool),
}

impl InterValue {
    fn to_real(&self) -> f64 {
        match self {
            Self::Integer(x) => *x as f64,
            Self::Real(x) => *x,
            Self::Character(x) => (*x as i64) as f64,
            Self::CharacterChain(x) => (!x.is_empty() as i64) as f64,
            Self::Boolean(x) => (*x as i64) as f64,
        }
    }

    fn to_integer(&self) -> i64 {
        match self {
            Self::Integer(x) => *x,
            Self::Real(x) => *x as i64,
            Self::Character(x) => *x as i64,
            Self::CharacterChain(x) => !x.is_empty() as i64,
            Self::Boolean(x) => *x as i64,
        }
    }

    fn to_boolean(&self) -> bool {
        match self {
            Self::Integer(x) => *x != 0,
            Self::Real(x) => *x != 0.0,
            Self::Character(x) => *x != '\0',
            Self::CharacterChain(x) => !x.is_empty(),
            Self::Boolean(x) => *x,
        }
    }

    fn pow(self, rhs: Self) -> Self {
        if matches!(self, Self::Real(_)) || matches!(rhs, Self::Real(_)) {
            Self::Real(self.to_real().powf(rhs.to_real()))
        } else if rhs.to_integer() < 0 {
            Self::Real(
                self.to_real()
                    .powi(rhs.to_integer().try_into().expect("Overflow")),
            )
        } else {
            Self::Integer(
                self.to_integer()
                    .pow(rhs.to_integer().try_into().expect("Overflow")),
            )
        }
    }
}

impl Mul for InterValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Real(_)) || matches!(rhs, Self::Real(_)) {
            Self::Real(self.to_real() * rhs.to_real())
        } else {
            Self::Integer(self.to_integer() * rhs.to_integer())
        }
    }
}

impl Div for InterValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Real(self.to_real() / rhs.to_real())
    }
}

impl Add for InterValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Real(_)) || matches!(rhs, Self::Real(_)) {
            Self::Real(self.to_real() + rhs.to_real())
        } else {
            Self::Integer(self.to_integer() + rhs.to_integer())
        }
    }
}

impl Sub for InterValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Real(_)) || matches!(rhs, Self::Real(_)) {
            Self::Real(self.to_real() - rhs.to_real())
        } else {
            Self::Integer(self.to_integer() - rhs.to_integer())
        }
    }
}

impl BitAnd for InterValue {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Boolean(_)) && matches!(rhs, Self::Boolean(_)) {
            Self::Boolean(self.to_boolean() && rhs.to_boolean())
        } else {
            Self::Integer(self.to_integer() & rhs.to_integer())
        }
    }
}

impl BitOr for InterValue {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Boolean(_)) && matches!(rhs, Self::Boolean(_)) {
            Self::Boolean(self.to_boolean() || rhs.to_boolean())
        } else {
            Self::Integer(self.to_integer() | rhs.to_integer())
        }
    }
}

impl BitXor for InterValue {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        if matches!(self, Self::Boolean(_)) && matches!(rhs, Self::Boolean(_)) {
            Self::Boolean(self.to_boolean() ^ rhs.to_boolean())
        } else {
            Self::Integer(self.to_integer() ^ rhs.to_integer())
        }
    }
}

impl Not for InterValue {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Boolean(b) => Self::Boolean(!b),
            _ => Self::Integer(!self.to_integer()),
        }
    }
}

impl ToString for InterValue {
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

fn evaluate_expression(
    expr: ExprTree,
    variables: &mut HashMap<Box<str>, InterValue>,
) -> InterValue {
    match expr.token {
        Token::Identifier(ident) => variables.get(&ident).unwrap().clone(),
        Token::IntLiteral(x) => InterValue::Integer(x),
        Token::RealLiteral(x) => InterValue::Real(x),
        Token::StringLiteral(x, _) => InterValue::CharacterChain(x),
        Token::True => InterValue::Boolean(true),
        Token::False => InterValue::Boolean(false),
        Token::Pow => evaluate_expression(*expr.left.unwrap(), variables)
            .pow(evaluate_expression(*expr.right.unwrap(), variables)),
        Token::Not => !evaluate_expression(*expr.left.unwrap(), variables),
        _ => panic!(),
    }
}

fn interpret_statement(statement: Statement, variables: &mut HashMap<Box<str>, InterValue>) {
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
                    InterValue::Integer(_) => {
                        variables.insert(ident, InterValue::Integer(buffer.trim().parse().unwrap()))
                    }
                    InterValue::Real(_) => {
                        variables.insert(ident, InterValue::Real(buffer.trim().parse().unwrap()))
                    }
                    InterValue::Character(_) => variables
                        .insert(ident, InterValue::Character(buffer.trim().parse().unwrap())),
                    InterValue::CharacterChain(_) => variables
                        .insert(ident, InterValue::CharacterChain(Box::from(buffer.trim()))),
                    InterValue::Boolean(_) => {
                        variables.insert(ident, InterValue::Boolean(buffer.trim().parse().unwrap()))
                    }
                };
                buffer.clear();
            }
        }
        _ => todo!(),
    }
}

pub fn interpret(program: Program) {
    let mut variables: HashMap<Box<str>, InterValue> = HashMap::new();
    for (ident, var) in program.variables {
        variables.insert(
            ident,
            match var.var_type {
                Type::Integer => InterValue::Integer(Default::default()),
                Type::Real => InterValue::Real(Default::default()),
                Type::Character => InterValue::Character(Default::default()),
                Type::CharacterChain => InterValue::CharacterChain(Default::default()),
                Type::Boolean => InterValue::Boolean(Default::default()),
            },
        );
    }
    for statement in program.statements {
        interpret_statement(statement, &mut variables);
    }
}
