use crate::parser::{ExprTree, Instruction, Program, Statement, Type};
use crate::tokenizer::Token;
use std::char::ParseCharError;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use std::str::ParseBoolError;
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

    fn get_type(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Real(_) => Type::Real,
            Self::Character(_) => Type::Character,
            Self::CharacterChain(_) => Type::CharacterChain,
            Self::Boolean(_) => Type::Boolean,
        }
    }
}

pub enum RuntimeError {
    MismatchedTypes,
    ReadError,
    UndeclaredIdentifier,
    StandardInputError,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedTypes => write!(f, "Há uma diferença entre os tipos das variáveis."),
            Self::ReadError => write!(f, "Formato inválido."),
            Self::UndeclaredIdentifier => {
                write!(f, "Há um identificador não declarado a ser usado.")
            }
            Self::StandardInputError => write!(f, "Erro a ler da standard input stream."),
        }
    }
}

macro_rules! impl_parse_errors {
    ($($error:ident),*) => {
        $(impl From<$error> for RuntimeError {
            fn from(_: $error) -> Self {
                Self::ReadError
            }
        })*
    };
}

impl_parse_errors!(
    ParseIntError,
    ParseFloatError,
    ParseCharError,
    ParseBoolError
);

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
) -> Result<InterValue, RuntimeError> {
    Ok(match &expr.token {
        Token::Identifier(ident) => variables
            .get(ident)
            .ok_or(RuntimeError::UndeclaredIdentifier)?
            .clone(),
        Token::IntLiteral(x) => InterValue::Integer(*x),
        Token::RealLiteral(x) => InterValue::Real(*x),
        Token::StringLiteral(x, _) => InterValue::CharacterChain(x.clone()),
        Token::True => InterValue::Boolean(true),
        Token::False => InterValue::Boolean(false),
        Token::Pow => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?.pow(
                evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?,
            )
        }
        Token::Not => {
            !evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::Mul => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                * evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::Div => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                / evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::IDiv => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                .idiv(evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?)
                .0
        }
        Token::Mod => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                .idiv(evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?)
                .1
        }
        Token::Plus => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                + evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::Minus => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                - evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::Less => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                < evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::Greater => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                > evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::LessOrEqual => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                <= evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::GreaterOrEqual => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                >= evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::Equal => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                == evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::Different => InterValue::Boolean(
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                != evaluate_expression(
                    unsafe { expr.right.as_ref().unwrap_unchecked() },
                    variables,
                )?,
        ),
        Token::And => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                & evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::Or => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                | evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        Token::XOr => {
            evaluate_expression(unsafe { expr.left.as_ref().unwrap_unchecked() }, variables)?
                ^ evaluate_expression(unsafe { expr.right.as_ref().unwrap_unchecked() }, variables)?
        }
        _ => unreachable!(),
    })
}

macro_rules! extract_integer {
    ($expr:ident, $variables:ident) => {
        match evaluate_expression($expr, $variables)? {
            InterValue::Integer(x) => x,
            _ => return Err(RuntimeError::MismatchedTypes),
        }
    };
}

fn interpret_statement(
    statement: &Statement,
    variables: &mut HashMap<Rc<str>, InterValue>,
) -> Result<(), RuntimeError> {
    match statement {
        Statement::SingleInstruction(Instruction::Assign(ident, expr)) => {
            let result = evaluate_expression(&expr, variables)?;
            let result_type = result.get_type();
            let prev = variables
                .insert(ident.clone(), result)
                .ok_or(RuntimeError::UndeclaredIdentifier)?;
            if prev.get_type() != result_type {
                return Err(RuntimeError::MismatchedTypes);
            }
        }
        Statement::SingleInstruction(Instruction::Write(tokens)) => {
            let mut buffer = String::new();
            for token in tokens {
                if let Token::StringLiteral(str, _) = token {
                    buffer.push_str(&str);
                } else if let Token::Identifier(ident) = token {
                    buffer.push_str(
                        &variables
                            .get(ident)
                            .ok_or(RuntimeError::UndeclaredIdentifier)?
                            .to_string(),
                    );
                } else {
                    buffer.push_str(&token.to_string());
                }
            }
            println!("{buffer}");
        }
        Statement::SingleInstruction(Instruction::Read(idents)) => {
            let mut buffer = String::new();
            for ident in idents {
                std::io::stdin()
                    .read_line(&mut buffer)
                    .map_err(|_| RuntimeError::StandardInputError)?;
                match &variables
                    .get(ident)
                    .ok_or(RuntimeError::UndeclaredIdentifier)?
                {
                    InterValue::Integer(_) => {
                        variables.insert(ident.clone(), InterValue::Integer(buffer.trim().parse()?))
                    }
                    InterValue::Real(_) => {
                        variables.insert(ident.clone(), InterValue::Real(buffer.trim().parse()?))
                    }
                    InterValue::Character(_) => variables
                        .insert(ident.clone(), InterValue::Character(buffer.trim().parse()?)),
                    InterValue::CharacterChain(_) => variables.insert(
                        ident.clone(),
                        InterValue::CharacterChain(Rc::from(buffer.trim())),
                    ),
                    InterValue::Boolean(_) => {
                        variables.insert(ident.clone(), InterValue::Boolean(buffer.trim().parse()?))
                    }
                };
                buffer.clear();
            }
        }
        Statement::IfStatement(condition, if_stat, else_stat) => {
            let condition = evaluate_expression(condition, variables)?;
            if let InterValue::Boolean(condition) = condition {
                if condition {
                    for stat in if_stat {
                        interpret_statement(stat, variables)?;
                    }
                } else {
                    if let Some(else_stat) = else_stat {
                        for stat in else_stat {
                            interpret_statement(stat, variables)?;
                        }
                    }
                }
            } else {
                return Err(RuntimeError::MismatchedTypes);
            }
        }
        Statement::WhileStatement(condition, while_stat) => {
            while {
                let condition = evaluate_expression(condition, variables)?;
                if let InterValue::Boolean(condition) = condition {
                    condition
                } else {
                    return Err(RuntimeError::MismatchedTypes);
                }
            } {
                for stat in while_stat {
                    interpret_statement(stat, variables)?;
                }
            }
        }
        Statement::DoWhileStatement(condition, while_stat) => loop {
            for stat in while_stat {
                interpret_statement(stat, variables)?;
            }
            let condition = evaluate_expression(condition, variables)?;
            if let InterValue::Boolean(condition) = condition {
                if !condition {
                    break;
                }
            } else {
                return Err(RuntimeError::MismatchedTypes);
            }
        },
        Statement::ForStatement(ident, start, end, step, for_stat) => {
            let mut i = extract_integer!(start, variables);
            let end = extract_integer!(end, variables);
            let step = extract_integer!(step, variables);
            let var = variables
                .get(ident)
                .ok_or(RuntimeError::UndeclaredIdentifier)?;
            if var.get_type() != Type::Integer {
                return Err(RuntimeError::MismatchedTypes);
            }
            while i <= end {
                variables.insert(ident.clone(), InterValue::Integer(i));
                for stat in for_stat {
                    interpret_statement(stat, variables)?;
                }
                i += step;
            }
        }
    }
    Ok(())
}

pub fn interpret(program: &Program) {
    let mut variables: HashMap<Rc<str>, InterValue> = HashMap::new();
    for (ident, var) in program.variables.iter() {
        variables.insert(
            ident.clone(),
            match var.var_type {
                Type::Integer => InterValue::Integer(Default::default()),
                Type::Real => InterValue::Real(Default::default()),
                Type::Character => InterValue::Character(Default::default()),
                Type::CharacterChain => InterValue::CharacterChain(Rc::from("")),
                Type::Boolean => InterValue::Boolean(Default::default()),
            },
        );
    }
    for statement in program.statements.iter() {
        match interpret_statement(statement, &mut variables) {
            Ok(()) => (),
            Err(err) => {
                println!("{err}");
                break;
            }
        }
    }
}
