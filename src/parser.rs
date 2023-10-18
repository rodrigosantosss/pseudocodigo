use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::tokenizer::Token;

pub enum ParseError {
    MissingAlgorithm(usize),
    ExpectedIdentifier(usize, String),
    ExpectedBreakLine(usize, String),
    ExpectedType(usize, String),
    Expected(usize, String, String),
    ExpectedExpression(usize, String),
    ExpectedOperation(usize),
    ExpectedToken(usize, Token, String),
    InvalidParenthesis(usize),
    Other(&'static str),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingAlgorithm(line) => {
                write!(f, "Falta a declaração do algoritmo na linha nº{line}")
            }
            Self::ExpectedIdentifier(line, found) => write!(
                f,
                "Esperava um identificador na linha nº{line}, encontrou-se {found}"
            ),
            Self::ExpectedBreakLine(line, found) => write!(
                f,
                "Esperava-se uma quebra de linha na linha nº{line}, encontrou-se {found}"
            ),
            Self::ExpectedType(line, found) => write!(
                f,
                "Esperava-se um tipo, encontrou-se {found} na linha nº{line}"
            ),
            Self::Expected(line, expected, found) => write!(
                f,
                "Esperava-se {expected}, encontrou-se {found} na linha nº{line}"
            ),
            Self::ExpectedToken(line, expected, found) => write!(
                f,
                "Esperava-se um {} na linha nº{line}, encontrou-se {}",
                expected.to_string(),
                found.to_string()
            ),
            Self::ExpectedExpression(line, found) => write!(
                f,
                "Esperava-se uma expressão na linha nº{line}, encontrou-se {}",
                found.to_string()
            ),
            Self::ExpectedOperation(line) => {
                write!(f, "Esperava-se uma operação na linha nº{line},")
            }
            Self::InvalidParenthesis(line) => write!(f, "Parenteses invalidos na linha nº{line}"),
            Self::Other(str) => write!(f, "{str}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprTree {
    pub token: Token,
    pub left: Option<Box<ExprTree>>,
    pub right: Option<Box<ExprTree>>,
}

impl ExprTree {
    fn new(token: Token) -> Self {
        Self {
            token,
            left: None,
            right: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Integer,
    Real,
    Character,
    CharacterChain,
    Boolean,
}

impl Type {
    fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Integer => Some(Type::Integer),
            Token::Real => Some(Type::Real),
            Token::Character => Some(Type::Character),
            Token::CharacterChain => Some(Type::CharacterChain),
            Token::Boolean => Some(Type::Boolean),
            _ => None,
        }
    }

    fn get_size(&self) -> usize {
        match self {
            Type::Integer | Type::Real | Type::CharacterChain => 8,
            Type::Character | Type::Boolean => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub offset: usize,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Assign(Rc<str>, ExprTree),
    Read(Vec<Rc<str>>),
    Write(Vec<Token>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    SingleInstruction(Instruction),
    IfStatement(ExprTree, Vec<Statement>, Option<Vec<Statement>>),
    WhileStatement(ExprTree, Vec<Statement>),
    ForStatement(Rc<str>, ExprTree, ExprTree, ExprTree, Vec<Statement>), // variable; start; end; step; code
    DoWhileStatement(ExprTree, Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub stack_size: usize,
    pub variables: HashMap<Rc<str>, Variable>,
    pub statements: Vec<Statement>,
}

pub enum Expression {
    Token(Token),
    Tree(ExprTree),
}

impl Expression {
    pub fn unwrap_token(self) -> Token {
        match self {
            Self::Token(token) => token,
            Self::Tree(_) => unreachable!(),
        }
    }
}

macro_rules! next_expression {
    ($iter:ident, $line:ident) => {
        match $iter.next() {
            Some(Expression::Token(Token::OpenParenthesis)) => {
                let mut expr: Vec<Expression> = Vec::new();
                let mut open_count: usize = 1;
                let mut close_count: usize = 0;
                loop {
                    match $iter.next() {
                        Some(Expression::Token(Token::OpenParenthesis)) => {
                            open_count += 1;
                            expr.push(Expression::Token(Token::OpenParenthesis));
                        }
                        Some(Expression::Token(Token::CloseParenthesis)) => {
                            close_count += 1;
                            if open_count == close_count {
                                break;
                            }
                            expr.push(Expression::Token(Token::OpenParenthesis));
                        }
                        Some(item) => expr.push(item),
                        None => break,
                    }
                }
                if close_count != open_count {
                    return Err(ParseError::InvalidParenthesis($line));
                }
                Some(Expression::Tree(parse_expression(expr, $line)?))
            }
            Some(Expression::Token(Token::CloseParenthesis)) => {
                return Err(ParseError::InvalidParenthesis($line))
            }
            Some(token) => Some(token),
            None => None,
        }
    };
}

macro_rules! parse_operation {
    ($iter:ident, $buffer:ident, $line:ident, $op:ident $(, $other_op:ident)*) => {
        while let Some(expr) = next_expression!($iter, $line) {
            match expr {
                Expression::Token(Token::$op $(| Token::$other_op)*) => {
                    let operation = expr.unwrap_token();
                    let last = match $buffer.pop() {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            } else {
                                return Err(ParseError::ExpectedExpression($line, token.to_string()));
                            }
                        }
                        Some(expr) => expr,
                        None => {
                            if let Token::Plus | Token::Minus = operation {
                                Expression::Token(Token::IntLiteral(0))
                            }
                            else {
                                return Err(ParseError::ExpectedExpression($line, String::from("nada")))
                            }
                        }
                    };
                    let next = match next_expression!($iter, $line) {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            } else {
                                return Err(ParseError::ExpectedExpression($line, token.to_string()));
                            }
                        }
                        Some(expr) => expr,
                        None => return Err(ParseError::ExpectedExpression($line, String::from("nada"))),
                    };
                    $buffer.push(Expression::Tree(ExprTree {
                        token: operation,
                        left: match last {
                            Expression::Token(lit) => Some(Box::new(ExprTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                        right: match next {
                            Expression::Token(lit) => Some(Box::new(ExprTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        }
                    }));
                },
                _ => $buffer.push(expr),
            }
        }
    };
}

macro_rules! parse_unary_operation {
    ($iter:ident, $buffer:ident, $line:ident, $op:ident $(, $other_op:ident)*) => {
        while let Some(expr) = next_expression!($iter, $line) {
            match expr {
                Expression::Token(Token::$op $(| Token::$other_op)*) => {
                    let operation = expr.unwrap_token();
                    let next = match next_expression!($iter, $line) {
                        Some(Expression::Token(token)) => {
                            if token.is_value() {
                                Expression::Token(token)
                            } else {
                                return Err(ParseError::ExpectedExpression($line, token.to_string()));
                            }
                        }
                        Some(expr) => expr,
                        None => return Err(ParseError::ExpectedExpression($line, String::from("nada"))),
                    };
                    $buffer.push(Expression::Tree(ExprTree {
                        token: operation,
                        left: match next {
                            Expression::Token(lit) => Some(Box::new(ExprTree::new(lit))),
                            Expression::Tree(tree) => Some(Box::new(tree)),
                        },
                        right: None
                    }));
                },
                _ => $buffer.push(expr),
            }
        }
    };
}

fn parse_expression(expr: Vec<Expression>, line: usize) -> Result<ExprTree, ParseError> {
    let mut expr = expr.into_iter();
    let mut buffer: Vec<Expression> = Vec::new();
    parse_unary_operation!(expr, buffer, line, Not);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, Pow);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, Mul, Div, IDiv, Mod);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, Plus, Minus);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(
        expr,
        buffer,
        line,
        Less,
        Greater,
        LessOrEqual,
        GreaterOrEqual
    );
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, Equal, Different);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, And);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, Or);
    expr = buffer.into_iter();
    buffer = Vec::new();
    parse_operation!(expr, buffer, line, XOr);
    if buffer.len() == 1 {
        Ok(
            match unsafe { buffer.into_iter().next().unwrap_unchecked() } {
                Expression::Token(token) => ExprTree::new(token),
                Expression::Tree(tree) => tree,
            },
        )
    } else if buffer.len() == 0 {
        Err(ParseError::ExpectedExpression(line, String::from("nada")))
    } else {
        Err(ParseError::ExpectedOperation(line))
    }
}

macro_rules! expected_token {
    ($iter:ident, $token:ident, $err:ident, $line:expr) => {
        match $iter.next() {
            Some(Token::$token) => (),
            Some(token) => return Err(ParseError::$err($line, token.to_string())),
            None => return Err(ParseError::$err($line, String::from("nada"))),
        }
    };
    ($iter:ident, $token:ident, $line:expr) => {
        match $iter.next() {
            Some(Token::$token) => (),
            Some(token) => {
                return Err(ParseError::ExpectedToken(
                    $line,
                    Token::$token,
                    token.to_string(),
                ))
            }
            None => {
                return Err(ParseError::ExpectedToken(
                    $line,
                    Token::$token,
                    String::from("nada"),
                ))
            }
        }
    };
}

fn parse_statements(tokens: Vec<Token>, line: &mut usize) -> Result<Vec<Statement>, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = Vec::new();

    while let Some(token) = tokens.next() {
        if let Token::Identifier(ident) = token {
            expected_token!(tokens, Arrow, *line);
            let mut expr = Vec::new();
            loop {
                let token = tokens
                    .next()
                    .ok_or(ParseError::ExpectedBreakLine(*line, String::from("nada")))?;
                if let Token::BreakLine = token {
                    break;
                } else {
                    expr.push(Expression::Token(token));
                }
            }
            statements.push(Statement::SingleInstruction(Instruction::Assign(
                ident,
                parse_expression(expr, *line)?,
            )));
            *line += 1;
        } else if let Token::Read = token {
            expected_token!(tokens, OpenParenthesis, *line);
            let mut identifiers: Vec<Rc<str>> = Vec::new();
            loop {
                identifiers.push(match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    Some(token) => {
                        return Err(ParseError::ExpectedIdentifier(*line, token.to_string()))
                    }
                    None => {
                        return Err(ParseError::ExpectedIdentifier(*line, String::from("nada")))
                    }
                });
                match tokens.peek() {
                    Some(Token::Comma) => tokens.next(),
                    _ => {
                        expected_token!(tokens, CloseParenthesis, *line);
                        break;
                    }
                };
            }
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            statements.push(Statement::SingleInstruction(Instruction::Read(identifiers)));
        } else if let Token::Write = token {
            expected_token!(tokens, OpenParenthesis, *line);
            let mut content: Vec<Token> = Vec::new();
            loop {
                content.push(match tokens.next() {
                    Some(token) => {
                        if token.is_value() {
                            token
                        } else {
                            return Err(ParseError::ExpectedIdentifier(*line, token.to_string()));
                        }
                    }
                    None => {
                        return Err(ParseError::ExpectedIdentifier(*line, String::from("nada")))
                    }
                });
                match tokens.peek() {
                    Some(Token::Comma) => tokens.next(),
                    _ => {
                        expected_token!(tokens, CloseParenthesis, *line);
                        break;
                    }
                };
            }
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            statements.push(Statement::SingleInstruction(Instruction::Write(content)));
        } else if let Token::If = token {
            let mut condition = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::Then) => break,
                    Some(token) => condition.push(Expression::Token(token)),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::Then.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let condition = parse_expression(condition, *line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            let mut content = Vec::new();
            let last: Token;
            let mut end_if_count = 0usize;
            let mut if_stmt_count = 0usize;
            loop {
                match tokens.next() {
                    Some(Token::EndIf) => {
                        end_if_count += 1;
                        if end_if_count > if_stmt_count {
                            last = Token::EndIf;
                            break;
                        }
                        content.push(Token::EndIf);
                    }
                    Some(Token::Else) => {
                        last = Token::Else;
                        break;
                    }
                    Some(Token::If) => {
                        if_stmt_count += 1;
                        content.push(Token::If);
                    }
                    Some(token) => content.push(token),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::EndIf.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let content = parse_statements(content, line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            let mut else_content: Option<Vec<Statement>> = None;
            end_if_count = 0usize;
            if_stmt_count = 0usize;
            if let Token::Else = last {
                let mut else_tokens = Vec::new();
                loop {
                    match tokens.next() {
                        Some(Token::EndIf) => {
                            end_if_count += 1;
                            if end_if_count > if_stmt_count {
                                break;
                            }
                            else_tokens.push(Token::EndIf);
                        }
                        Some(Token::If) => {
                            if_stmt_count += 1;
                            else_tokens.push(Token::If);
                        }
                        Some(token) => else_tokens.push(token),
                        None => {
                            return Err(ParseError::Expected(
                                *line,
                                Token::EndIf.to_string(),
                                String::from("nada"),
                            ))
                        }
                    };
                }
                else_content = Some(parse_statements(else_tokens, line)?);
                expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            }
            *line += 1;
            statements.push(Statement::IfStatement(condition, content, else_content));
        } else if let Token::While = token {
            let mut condition = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::Do(str)) => {
                        if &*str == "faça" {
                            break;
                        }
                        condition.push(Expression::Token(Token::Do(str)));
                    }
                    Some(token) => condition.push(Expression::Token(token)),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::Do(Box::from("faça")).to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let condition = parse_expression(condition, *line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            let mut content = Vec::new();
            let mut end_while_count = 0usize;
            let mut while_stmt_count = 0usize;
            loop {
                match tokens.next() {
                    Some(Token::EndWhile) => {
                        end_while_count += 1;
                        if end_while_count > while_stmt_count {
                            break;
                        }
                        content.push(Token::EndWhile);
                    }
                    Some(Token::Do(str)) => {
                        if &*str == "Repita" {
                            while_stmt_count += 1;
                        }
                    }
                    Some(Token::While) => {
                        while_stmt_count += 1;
                        content.push(Token::While);
                    }
                    Some(token) => content.push(token),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::EndWhile.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let content = parse_statements(content, line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            statements.push(Statement::WhileStatement(condition, content));
        } else if let Token::Do(str) = token {
            if &*str != "Repita" {
                return Err(ParseError::Other("Faça mal posicionado."));
            }
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            let mut content = Vec::new();
            let mut end_while_count = 0usize;
            let mut while_stmt_count = 0usize;
            loop {
                match tokens.next() {
                    Some(Token::EndWhile) => {
                        end_while_count += 1;
                        content.push(Token::EndWhile);
                    }
                    Some(Token::While) => {
                        while_stmt_count += 1;
                        if while_stmt_count > end_while_count {
                            break;
                        }
                        content.push(Token::While);
                    }
                    Some(token) => content.push(token),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::While.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let content = parse_statements(content, line)?;
            let mut condition = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::End) => break,
                    Some(token) => condition.push(Expression::Token(token)),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::End.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let condition = parse_expression(condition, *line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            statements.push(Statement::DoWhileStatement(condition, content));
        } else if let Token::For = token {
            let ident = match tokens
                .next()
                .ok_or(ParseError::ExpectedIdentifier(*line, String::from("nada")))?
            {
                Token::Identifier(ident) => ident,
                token => return Err(ParseError::ExpectedIdentifier(*line, token.to_string())),
            };
            expected_token!(tokens, From, *line);
            let mut start_expr = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::To) => break,
                    Some(token) => start_expr.push(Expression::Token(token)),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::To.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let start_expr = parse_expression(start_expr, *line)?;
            let mut end_expr = Vec::new();
            let last: Token;
            loop {
                match tokens.next() {
                    Some(Token::Step) => {
                        last = Token::Step;
                        break;
                    }
                    Some(Token::Do(str)) => {
                        if &*str == "faça" {
                            last = Token::Do(str);
                            break;
                        } else {
                            end_expr.push(Expression::Token(Token::Do(str)));
                        }
                    }
                    Some(token) => end_expr.push(Expression::Token(token)),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::Do(Box::from("faça")).to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let end_expr = parse_expression(end_expr, *line)?;
            let mut step_expr: Option<ExprTree> = None;
            if let Token::Step = last {
                let mut step_expr_vec = Vec::new();
                loop {
                    match tokens.next() {
                        Some(Token::Do(str)) => {
                            if &*str == "faça" {
                                break;
                            } else {
                                step_expr_vec.push(Expression::Token(Token::Do(str)));
                            }
                        }
                        Some(token) => step_expr_vec.push(Expression::Token(token)),
                        None => {
                            return Err(ParseError::Expected(
                                *line,
                                Token::Do(Box::from("faça")).to_string(),
                                String::from("nada"),
                            ))
                        }
                    };
                }
                step_expr = Some(parse_expression(step_expr_vec, *line)?);
            }
            let step_expr = step_expr.unwrap_or(ExprTree::new(Token::IntLiteral(1)));
            let mut content = Vec::new();
            let mut end_for_count = 0usize;
            let mut for_stmt_count = 0usize;
            loop {
                match tokens.next() {
                    Some(Token::EndFor) => {
                        end_for_count += 1;
                        if end_for_count > for_stmt_count {
                            break;
                        }
                        content.push(Token::EndFor);
                    }
                    Some(Token::For) => {
                        for_stmt_count += 1;
                        content.push(Token::For);
                    }
                    Some(token) => content.push(token),
                    None => {
                        return Err(ParseError::Expected(
                            *line,
                            Token::EndFor.to_string(),
                            String::from("nada"),
                        ))
                    }
                };
            }
            let content = parse_statements(content, line)?;
            expected_token!(tokens, BreakLine, ExpectedBreakLine, *line);
            *line += 1;
            statements.push(Statement::ForStatement(
                ident, start_expr, end_expr, step_expr, content,
            ));
        } else if let Token::BreakLine = token {
            *line += 1;
        } else if let Token::End = token {
            match tokens.next().ok_or(ParseError::Expected(
                *line,
                Token::Dot.to_string(),
                String::from("nada"),
            ))? {
                Token::Dot => (),
                token => {
                    return Err(ParseError::Expected(
                        *line,
                        Token::Dot.to_string(),
                        token.to_string(),
                    ))
                }
            };
            while let Some(token) = tokens.next() {
                match token {
                    Token::BreakLine => (),
                    _ => return Err(ParseError::Other("Tokens inválidos após Fim.")),
                }
            }
            break;
        }
    }

    Ok(statements)
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut iterator = tokens.into_iter().peekable();
    let mut line: usize = 1;

    match iterator.next() {
        Some(Token::Algorithm) => (),
        _ => return Err(ParseError::MissingAlgorithm(line)),
    }

    match iterator.next() {
        Some(Token::Identifier(_)) => (),
        Some(token) => return Err(ParseError::ExpectedIdentifier(line, token.to_string())),
        None => return Err(ParseError::ExpectedIdentifier(line, String::from("nada"))),
    }

    let mut program = Program {
        stack_size: 0,
        variables: HashMap::new(),
        statements: Vec::new(),
    };

    expected_token!(iterator, BreakLine, ExpectedBreakLine, line);
    line += 1;

    match iterator.next() {
        Some(Token::Data) => {
            expected_token!(iterator, BreakLine, ExpectedBreakLine, line);
            line += 1;
            let mut offset: usize = 0;
            let mut identifiers: Vec<Rc<str>> = Vec::new();
            let mut curr_type: Option<Token> = None;
            loop {
                let ident = match iterator.next() {
                    Some(Token::Begin) => break,
                    Some(Token::BreakLine) => {
                        let var_type = match curr_type {
                            Some(token) => {
                                if let Some(var_type) = Type::from_token(token.clone()) {
                                    var_type
                                } else {
                                    return Err(ParseError::ExpectedType(line, token.to_string()));
                                }
                            }
                            None => {
                                return Err(ParseError::ExpectedType(line, String::from("nada")))
                            }
                        };
                        let type_size = var_type.get_size();
                        program.stack_size += type_size * identifiers.len();
                        for identifier in identifiers {
                            program
                                .variables
                                .insert(identifier, Variable { offset, var_type });
                            offset += type_size;
                        }
                        identifiers = Vec::new();
                        curr_type = None;
                        line += 1;
                        continue;
                    }
                    Some(Token::Identifier(ident)) => ident,
                    Some(token) => {
                        return Err(ParseError::ExpectedIdentifier(line, token.to_string()))
                    }
                    None => return Err(ParseError::ExpectedIdentifier(line, String::from("nada"))),
                };
                identifiers.push(ident);
                match iterator.next() {
                    Some(Token::Comma) => continue,
                    Some(Token::Colon) => (),
                    Some(token) => {
                        return Err(ParseError::Expected(
                            line,
                            String::from(":"),
                            token.to_string(),
                        ))
                    }
                    None => {
                        return Err(ParseError::Expected(
                            line,
                            String::from(":"),
                            String::from("nada"),
                        ))
                    }
                };
                curr_type = iterator.next();
                match iterator.peek() {
                    Some(Token::BreakLine) => (),
                    Some(token) => {
                        return Err(ParseError::ExpectedBreakLine(line, token.to_string()))
                    }
                    None => return Err(ParseError::ExpectedBreakLine(line, String::from("nada"))),
                }
            }
        }
        Some(Token::Begin) => (),
        Some(token) => {
            return Err(ParseError::Expected(
                line,
                String::from("Início ou Fim"),
                token.to_string(),
            ))
        }
        None => {
            return Err(ParseError::Expected(
                line,
                String::from("Início ou Fim"),
                String::from("nada"),
            ))
        }
    }

    expected_token!(iterator, BreakLine, ExpectedBreakLine, line);
    line += 1;

    let mut tokens = Vec::new();
    tokens.extend(iterator);
    program.statements = parse_statements(tokens, &mut line)?;
    Ok(program)
}
