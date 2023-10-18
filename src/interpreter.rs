use crate::parser::{ExprTree, Instruction, Program, Statement, Type};
use crate::tokenizer::Token;
use std::rc::Rc;
use std::{
    collections::HashMap, ops::Add, ops::BitAnd, ops::BitOr, ops::BitXor, ops::Div, ops::Mul,
    ops::Not, ops::Rem, ops::Sub,
};

#[derive(Clone)]
enum InterValue {
    Integer(i64),
    Real(f64),
    Character(char),
    CharacterChain(Rc<str>),
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

    fn idiv(self, rhs: Self) -> (Self, Self) {
        if matches!(self, Self::Real(_)) || matches!(rhs, Self::Real(_)) {
            let x = self.to_real();
            let y = self.to_real();
            (Self::Integer((x / y).floor() as i64), Self::Real(x.rem(y)))
        } else {
            let x = self.to_integer();
            let y = self.to_integer();
            (Self::Integer(x / y), Self::Integer(x % y))
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

impl PartialEq for InterValue {
    fn eq(&self, other: &Self) -> bool {
        if matches!(self, Self::CharacterChain(_)) && matches!(self, Self::CharacterChain(_)) {
            let str1 = match self {
                Self::CharacterChain(x) => x,
                _ => unreachable!(),
            };
            let str2 = match other {
                Self::CharacterChain(x) => x,
                _ => unreachable!(),
            };
            str1 == str2
        } else if matches!(self, Self::Real(_)) && matches!(other, Self::Real(_)) {
            self.to_real() == other.to_real()
        } else {
            self.to_integer() == other.to_integer()
        }
    }
}

impl PartialOrd for InterValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if matches!(self, Self::CharacterChain(_)) && matches!(self, Self::CharacterChain(_)) {
            let str1 = match self {
                Self::CharacterChain(x) => x,
                _ => unreachable!(),
            };
            let str2 = match other {
                Self::CharacterChain(x) => x,
                _ => unreachable!(),
            };
            str1.partial_cmp(str2)
        } else if matches!(self, Self::Real(_)) && matches!(other, Self::Real(_)) {
            self.to_real().partial_cmp(&other.to_real())
        } else {
            self.to_integer().partial_cmp(&other.to_integer())
        }
    }
}

impl ToString for InterValue {
    fn to_string(&self) -> String {
        match self {
            Self::Integer(x) => x.to_string(),
            Self::Real(x) => x.to_string(),
            Self::Character(x) => String::from(*x),
            Self::CharacterChain(x) => x.to_string(),
            Self::Boolean(x) => x.to_string(),
        }
    }
}

fn evaluate_expression(
    expr: &ExprTree,
    variables: &mut HashMap<Rc<str>, InterValue>,
) -> InterValue {
    match &expr.token {
        Token::Identifier(ident) => variables.get(ident).unwrap().clone(),
        Token::IntLiteral(x) => InterValue::Integer(*x),
        Token::RealLiteral(x) => InterValue::Real(*x),
        Token::StringLiteral(x, _) => InterValue::CharacterChain(x.clone()),
        Token::True => InterValue::Boolean(true),
        Token::False => InterValue::Boolean(false),
        Token::Pow => evaluate_expression(expr.left.as_ref().unwrap(), variables)
            .pow(evaluate_expression(expr.right.as_ref().unwrap(), variables)),
        Token::Not => !evaluate_expression(expr.left.as_ref().unwrap(), variables),
        Token::Mul => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                * evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::Div => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                / evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::IDiv => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                .idiv(evaluate_expression(expr.right.as_ref().unwrap(), variables))
                .0
        }
        Token::Mod => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                .idiv(evaluate_expression(expr.right.as_ref().unwrap(), variables))
                .1
        }
        Token::Plus => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                + evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::Minus => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                - evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::Less => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                < evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::Greater => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                > evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::LessOrEqual => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                <= evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::GreaterOrEqual => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                >= evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::Equal => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                == evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::Different => InterValue::Boolean(
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                != evaluate_expression(expr.right.as_ref().unwrap(), variables),
        ),
        Token::And => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                & evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::Or => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                | evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        Token::XOr => {
            evaluate_expression(expr.left.as_ref().unwrap(), variables)
                ^ evaluate_expression(expr.right.as_ref().unwrap(), variables)
        }
        _ => unimplemented!(),
    }
}

fn interpret_statement(statement: &Statement, variables: &mut HashMap<Rc<str>, InterValue>) {
    match statement {
        Statement::SingleInstruction(Instruction::Assign(ident, expr)) => {
            let result = evaluate_expression(&expr, variables);
            variables.insert(ident.clone(), result);
        }
        Statement::SingleInstruction(Instruction::Write(tokens)) => {
            let mut buffer = String::new();
            for token in tokens {
                if let Token::StringLiteral(str, _) = token {
                    buffer.push_str(&str);
                } else if let Token::Identifier(ident) = token {
                    buffer.push_str(&variables.get(ident).unwrap().to_string());
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
                match &variables.get(ident).unwrap() {
                    InterValue::Integer(_) => variables.insert(
                        ident.clone(),
                        InterValue::Integer(buffer.trim().parse().unwrap()),
                    ),
                    InterValue::Real(_) => variables.insert(
                        ident.clone(),
                        InterValue::Real(buffer.trim().parse().unwrap()),
                    ),
                    InterValue::Character(_) => variables.insert(
                        ident.clone(),
                        InterValue::Character(buffer.trim().parse().unwrap()),
                    ),
                    InterValue::CharacterChain(_) => variables.insert(
                        ident.clone(),
                        InterValue::CharacterChain(Rc::from(buffer.trim())),
                    ),
                    InterValue::Boolean(_) => variables.insert(
                        ident.clone(),
                        InterValue::Boolean(buffer.trim().parse().unwrap()),
                    ),
                };
                buffer.clear();
            }
        }
        Statement::IfStatement(condition, if_stat, else_stat) => {
            if evaluate_expression(condition, variables).to_boolean() {
                for stat in if_stat {
                    interpret_statement(stat, variables);
                }
            } else {
                if let Some(else_stat) = else_stat {
                    for stat in else_stat {
                        interpret_statement(stat, variables);
                    }
                }
            }
        }
        Statement::WhileStatement(condition, while_stat) => {
            while evaluate_expression(condition, variables).to_boolean() {
                for stat in while_stat {
                    interpret_statement(stat, variables);
                }
            }
        }
        Statement::DoWhileStatement(condition, while_stat) => loop {
            for stat in while_stat {
                interpret_statement(stat, variables);
            }
            if !evaluate_expression(condition, variables).to_boolean() {
                break;
            }
        },
        Statement::ForStatement(ident, start, end, step, for_stat) => {
            let mut i = evaluate_expression(start, variables).to_integer();
            let end = evaluate_expression(end, variables).to_integer();
            let step = evaluate_expression(step, variables).to_integer();
            while i <= end {
                variables.insert(ident.clone(), InterValue::Integer(i));
                for stat in for_stat {
                    interpret_statement(stat, variables);
                }
                i += step;
            }
        }
    }
}

pub fn interpret(program: Program) {
    let mut variables: HashMap<Rc<str>, InterValue> = HashMap::new();
    for (ident, var) in program.variables {
        variables.insert(
            ident,
            match var.var_type {
                Type::Integer => InterValue::Integer(Default::default()),
                Type::Real => InterValue::Real(Default::default()),
                Type::Character => InterValue::Character(Default::default()),
                Type::CharacterChain => InterValue::CharacterChain(Rc::from("")),
                Type::Boolean => InterValue::Boolean(Default::default()),
            },
        );
    }
    for statement in program.statements {
        interpret_statement(&statement, &mut variables);
    }
}
