use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Token {
    Algorithm, // Algoritmo
    Data,      // Variáveis
    Begin,     // Inicio
    End,       // Fim
    Dot,
    If,           // Se
    Then,         // então
    Else,         // Senão
    While,        // Enquanto,
    For,          // Para
    Do(Box<str>), // faça, repita
    EndIf,
    EndWhile,
    EndFor,
    EndDo,
    From, // de
    To,   // até
    Step, // passo
    BreakLine,
    OpenParenthesis,
    CloseParenthesis,
    Identifier(Rc<str>),
    Arrow, // <-
    Colon,
    IntLiteral(i64),
    RealLiteral(f64),
    StringLiteral(Rc<str>, char),
    Integer,        // inteiro
    Real,           // real
    Character,      // caractere
    CharacterChain, // cadeia
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
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    Different, // <>
    And,       // e
    Or,        // ou
    XOr,       // xor
    Not,       // não
    Write,     // Escrever
    Read,      // Ler
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "Algoritmo" => Self::Algorithm,
            "Variáveis" => Self::Data,
            "Início" => Self::Begin,
            "Fim" => Self::End,
            "Se" => Self::If,
            "Então" => Self::Then,
            "Senão" => Self::Else,
            "Enquanto" => Self::While,
            "Para" => Self::For,
            "faça" | "Repita" => Self::Do(value.into_boxed_str()),
            "FimSe" => Self::EndIf,
            "FimEnquanto" => Self::EndWhile,
            "FimPara" => Self::EndFor,
            "FimRepita" => Self::EndDo,
            "de" => Self::From,
            "até" => Self::To,
            "passo" => Self::Step,
            "inteiro" => Self::Integer,
            "real" => Self::Real,
            "caractere" => Self::Character,
            "cadeia" => Self::CharacterChain,
            "lógico" => Self::Boolean,
            "verdadeiro" => Self::True,
            "falso" => Self::False,
            "div" => Self::IDiv,
            "mod" => Self::Mod,
            "e" => Self::And,
            "ou" => Self::Or,
            "xor" => Self::XOr,
            "não" => Self::Not,
            "Escrever" => Self::Write,
            "Ler" => Self::Read,
            _ => Self::Identifier(value.into()),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = ();

    fn try_from(value: char) -> Result<Token, ()> {
        match value {
            '\n' => Ok(Self::BreakLine),
            '.' => Ok(Self::Dot),
            '(' => Ok(Self::OpenParenthesis),
            ')' => Ok(Self::CloseParenthesis),
            ':' => Ok(Self::Colon),
            '+' => Ok(Self::Plus),
            '-' => Ok(Self::Minus),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            '^' => Ok(Self::Pow),
            ',' => Ok(Self::Comma),
            '=' => Ok(Self::Equal),
            _ => Err(()),
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Self::Algorithm => String::from("Algoritmo"),
            Self::Data => String::from("Variáveis"),
            Self::Begin => String::from("Início"),
            Self::End => String::from("Fim"),
            Self::Dot => String::from("."),
            Self::If => String::from("Se"),
            Self::Then => String::from("então"),
            Self::Else => String::from("Senão"),
            Self::While => String::from("Enquanto"),
            Self::For => String::from("Para"),
            Self::Do(str) => str.clone().into_string(),
            Self::EndIf => String::from("FimSe"),
            Self::EndWhile => String::from("FimEnquanto"),
            Self::EndFor => String::from("FimPara"),
            Self::EndDo => String::from("FimRepita"),
            Self::From => String::from("de"),
            Self::To => String::from("até"),
            Self::Step => String::from("passo"),
            Self::Integer => String::from("inteiro"),
            Self::Real => String::from("real"),
            Self::Character => String::from("caractere"),
            Self::CharacterChain => String::from("cadeia"),
            Self::Boolean => String::from("lógico"),
            Self::True => String::from("verdadeiro"),
            Self::False => String::from("falso"),
            Self::IDiv => String::from("div"),
            Self::Mod => String::from("mod"),
            Self::And => String::from("e"),
            Self::Or => String::from("ou"),
            Self::XOr => String::from("xor"),
            Self::Not => String::from("não"),
            Self::Arrow => String::from("<-"),
            Self::Different => String::from("<>"),
            Self::Less => String::from("<"),
            Self::Greater => String::from(">"),
            Self::LessOrEqual => String::from("<="),
            Self::GreaterOrEqual => String::from(">="),
            Self::BreakLine => String::from("\n"),
            Self::OpenParenthesis => String::from("("),
            Self::CloseParenthesis => String::from(")"),
            Self::Colon => String::from(":"),
            Self::Plus => String::from("+"),
            Self::Minus => String::from("-"),
            Self::Mul => String::from("*"),
            Self::Div => String::from("/"),
            Self::Pow => String::from("^"),
            Self::Comma => String::from(","),
            Self::Equal => String::from("="),
            Self::Write => String::from("Escrever"),
            Self::Read => String::from("Ler"),
            Self::Identifier(str) => str.to_string(),
            Self::RealLiteral(real) => real.to_string(),
            Self::IntLiteral(integer) => integer.to_string(),
            Self::StringLiteral(literal, quote) => {
                let mut str = String::new();
                str.push(*quote);
                str.push_str(&literal);
                str.push(*quote);
                str
            }
        }
    }
}

impl Token {
    pub fn is_value(&self) -> bool {
        match self {
            Token::IntLiteral(_)
            | Token::RealLiteral(_)
            | Token::StringLiteral(_, _)
            | Token::Identifier(_)
            | Token::True
            | Token::False => true,
            _ => false,
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
    let mut line: usize = 1;
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
                Some('=') => {
                    iterator.next();
                    char += 1;
                    tokens.push(Token::LessOrEqual);
                }
                Some(_) | None => tokens.push(Token::Less),
            }
        } else if c == '>' {
            match iterator.peek() {
                Some('=') => {
                    iterator.next();
                    char += 1;
                    tokens.push(Token::GreaterOrEqual);
                }
                Some(_) | None => tokens.push(Token::Greater),
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
            tokens.push(Token::StringLiteral(buffer.into(), c));
        } else if !c.is_whitespace() {
            let token: Token = c
                .try_into()
                .map_err(|_| TokenizeError::InvalidToken(line, char, String::from(c)))?;
            tokens.push(token);
        }
    }

    Ok(tokens)
}
