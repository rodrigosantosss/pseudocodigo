use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

#[derive(Debug)]
pub enum Token {
    Algorithm, // Algoritmo
    Data,      // Dados
    Begin,     // Inicio
    End,       // Fim
    If,        // Se
    Then,      // então
    Else,      // Senão
    While,     // Enquanto
    BreakLine,
    OpenParenthesis,
    CloseParenthesis,
    Identifier(Box<str>),
    Arrow, // <-
    Colon,
    IntLiteral(i64),
    RealLiteral(f64),
    StringLiteral(Box<str>),
    Integer,        // inteiro
    Real,           // real
    Character,      // caractere
    CharacterChain, // literal
    Boolean,        // lógico
    True,           // verdadeiro
    False,          // falso
    Plus,
    Minus,
    Mul,
    Div,
    IDiv, // div
    Mod,  // mod
    Pow,
    Comma,
    Less,
    Greater,
    Equal,
    Different, // <>
    And, // e
    Or,  // ou
    XOr, // xor
    Not, // não
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "Algoritmo" => Self::Algorithm,
            "Dados" => Self::Data,
            "Início" => Self::Begin,
            "Fim" => Self::End,
            "Se" => Self::If,
            "então" => Self::Then,
            "Senão" => Self::Else,
            "Enquanto" => Self::While,
            "inteiro" => Self::Integer,
            "real" => Self::Real,
            "caractere" => Self::Character,
            "literal" => Self::CharacterChain,
            "lógico" => Self::Boolean,
            "verdadeiro" => Self::True,
            "falso" => Self::False,
            "div" => Self::IDiv,
            "mod" => Self::Mod,
            "e" => Self::And,
            "ou" => Self::Or,
            "xor" => Self::XOr,
            "não" => Self::Not,
            _ => Self::Identifier(value.into_boxed_str()),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = ();

    fn try_from(value: char) -> Result<Token, ()> {
        match value {
            '\n' => Ok(Self::BreakLine),
            '(' => Ok(Self::OpenParenthesis),
            ')' => Ok(Self::CloseParenthesis),
            ':' => Ok(Self::Colon),
            '+' => Ok(Self::Plus),
            '-' => Ok(Self::Minus),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            '^' => Ok(Self::Pow),
            ',' => Ok(Self::Comma),
            '>' => Ok(Self::Greater),
            '=' => Ok(Self::Equal),
            _ => Err(()),
        }
    }
}

pub enum TokenizeError {
    InvalidToken(usize, usize, String),
    RealLiteralParseError(usize, usize, String, ParseFloatError),
    IntLiteralParseError(usize, usize, String, ParseIntError),
    ExpectedCharacter(usize, usize, String),
    MissingEndQuotes(usize, usize, String),
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidToken(line, char, content) => {
                write!(f, "Token inválido em {line}:{char} \"{content}\"")
            }
            Self::RealLiteralParseError(line, char, content, err) => {
                write!(
                    f,
                    "Literal real inválido em {line}:{char} \"{content}\": {err}"
                )
            }
            Self::IntLiteralParseError(line, char, content, err) => {
                write!(
                    f,
                    "Literal inteiro inválido em {line}:{char} \"{content}\": {err}"
                )
            }
            Self::ExpectedCharacter(line, char, content) => {
                write!(f, "Falta um caractere em {line}:{char} \"{content}\"")
            }
            Self::MissingEndQuotes(line, char, content) => {
                write!(f, "Falta umas aspas em {line}:{char} \"{content}\"")
            }
        }
    }
}

pub fn tokenize(code: String) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    let mut iterator = code.chars().peekable();
    let mut line: usize = 0;
    let mut char: usize = 0;

    while let Some(c) = iterator.next() {
        char += 1;
        if c == '\n' {
            line += 1;
            char = 0;
            tokens.push(Token::BreakLine);
        } else if c == '<' {
            match iterator.peek() {
                Some('-') => {
                    iterator.next();
                    char += 1;
                    tokens.push(Token::Arrow);
                }
                Some('>') => {
                    iterator.next();
                    char += 1;
                    tokens.push(Token::Different);
                }
                Some(_) | None => tokens.push(Token::Less),
            }
        } else if c.is_alphabetic() {
            let mut buffer = String::from(c);
            while let Some(&d) = iterator.peek() {
                if !d.is_digit(10) && d != '_' && !d.is_alphabetic() {
                    break;
                }
                iterator.next();
                char += 1;
                buffer.push(d);
            }
            let token: Token = buffer.into();
            tokens.push(token);
        } else if c.is_digit(10) {
            let mut buffer = String::from(c);
            let mut has_dot = false;
            while let Some(&d) = iterator.peek() {
                if d == '.' {
                    buffer.push('.');
                    if has_dot {
                        return Err(TokenizeError::InvalidToken(line, char, buffer));
                    } else {
                        iterator.next();
                        char += 1;
                        has_dot = true;
                    }
                } else if d.is_digit(10) {
                    iterator.next();
                    char += 1;
                    buffer.push(d);
                } else {
                    break;
                }
            }
            let token =
                if has_dot {
                    Token::RealLiteral(buffer.parse().map_err(|err| {
                        TokenizeError::RealLiteralParseError(line, char, buffer, err)
                    })?)
                } else {
                    Token::IntLiteral(buffer.parse().map_err(|err| {
                        TokenizeError::IntLiteralParseError(line, char, buffer, err)
                    })?)
                };
            tokens.push(token);
        } else if c == '"' || c == '\'' {
            let mut buffer = String::new();
            loop {
                if let Some(d) = iterator.next() {
                    if d == '\\' {
                        match iterator.next() {
                            Some(e) => {
                                buffer.push(e);
                                char += 2;
                            }
                            None => {
                                buffer.push(d);
                                return Err(TokenizeError::ExpectedCharacter(line, char, buffer));
                            }
                        }
                        continue;
                    }
                    char += 1;
                    if c == d {
                        break;
                    }
                    buffer.push(d);
                } else {
                    return Err(TokenizeError::MissingEndQuotes(line, char, buffer));
                }
            }
            tokens.push(Token::StringLiteral(buffer.into_boxed_str()));
        } else if !c.is_whitespace() {
            let token: Token = c
                .try_into()
                .map_err(|_| TokenizeError::InvalidToken(line, char, String::from(c)))?;
            tokens.push(token);
        }
    }

    Ok(tokens)
}
