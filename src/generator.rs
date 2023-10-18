use crate::parser::{ExprTree, Instruction, Program, Statement, Type, Variable};
use crate::tokenizer::Token;
use std::{collections::HashMap, rc::Rc};

const REGISTERS: [&str; 6] = [ "rdi", "rsi", "rdx", "rcx", "r8", "r9" ];
const REGISTERS_8: [&str; 6] = [ "dil", "sil", "dl", "cl", "r8b", "r9b" ];

#[derive(Debug)]
pub enum GenerationError {
    IdentifierNotDeclared,
}

fn generate_expression(
    expression: ExprTree,
    variables: &HashMap<Rc<str>, Variable>,
    instructions: &mut Vec<Box<str>>,
    program_data: &mut ProgramData,
) -> Result<(), GenerationError> {
    match expression.token {
        Token::Identifier(ident) => {
            let var = variables
                .get(&ident)
                .ok_or(GenerationError::IdentifierNotDeclared)?;
            match var.var_type {
                Type::Real | Type::Integer | Type::CharacterChain => instructions
                    .push(format!("\tmov rax, qword ptr [rbp-{}]", var.offset).into_boxed_str()),
                Type::Boolean | Type::Character => {
                    instructions.push(Box::from("\txor rax, rax"));
                    instructions.push(format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                }
            }
            instructions.push(Box::from("\tpush rax"));
        }
        Token::IntLiteral(x) => {
            instructions.push(format!("\tmov rax, {x}").into_boxed_str());
            instructions.push(Box::from("\tpush rax"));
        }
        Token::StringLiteral(content, _) => {
            let i = program_data.strings.len();
            program_data.strings.push((*content).into());
            instructions.push(format!("\tlea rax, [str{i}]").into_boxed_str());
            instructions.push(Box::from("\tpush rax"));
        }
        Token::True => {
            instructions.push(Box::from("\tmov rax, 1"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::False => {
            instructions.push(Box::from("\txor rax, rax"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::Pow => {
            program_data.pow = true;
            generate_expression(*expression.left.unwrap(), variables, instructions, program_data)?;
            generate_expression(*expression.right.unwrap(), variables, instructions, program_data)?;
            instructions.pop();
            instructions.push(Box::from("\tmov rsi, rax"));
            instructions.push(Box::from("\tpop rdi"));
            instructions.push(Box::from("\tcall pow"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::Not => {
            generate_expression(*expression.left.unwrap(), variables, instructions, program_data)?;
            instructions.pop();
            instructions.push(Box::from("\tnot rax"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::Mul => {
            generate_expression(*expression.left.unwrap(), variables, instructions, program_data)?;
            generate_expression(*expression.right.unwrap(), variables, instructions, program_data)?;
            instructions.pop();
            instructions.push(Box::from("\tpop rdi"));
            instructions.push(Box::from("\timul rax, rdi"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::Plus => {
            generate_expression(*expression.left.unwrap(), variables, instructions, program_data)?;
            generate_expression(*expression.right.unwrap(), variables, instructions, program_data)?;
            instructions.pop();
            instructions.push(Box::from("\tpop rdi"));
            instructions.push(Box::from("\tadd rax, rdi"));
            instructions.push(Box::from("\tpush rax"));
        }
        Token::Minus => {
            generate_expression(*expression.left.unwrap(), variables, instructions, program_data)?;
            generate_expression(*expression.right.unwrap(), variables, instructions, program_data)?;
            instructions.pop();
            instructions.push(Box::from("\tmov rdi, rax"));
            instructions.push(Box::from("\tpop rax"));
            instructions.push(Box::from("\tsub rax, rdi"));
            instructions.push(Box::from("\tpush rax"));
        }
        _ => unimplemented!(),
    }
    Ok(())
}

fn generate_statement(
    statement: Statement,
    variables: &HashMap<Rc<str>, Variable>,
    instructions: &mut Vec<Box<str>>,
    program_data: &mut ProgramData,
) -> Result<(), GenerationError> {
    match statement {
        Statement::SingleInstruction(Instruction::Assign(ident, expression)) => {
            generate_expression(expression, variables, instructions, program_data)?;
            instructions.pop();
            let var = variables
                .get(&ident)
                .ok_or(GenerationError::IdentifierNotDeclared)?;
            instructions.push(
                match var.var_type {
                    Type::Real | Type::Integer | Type::CharacterChain => {
                        format!("\tmov qword ptr [rbp-{}], rax", var.offset)
                    }
                    Type::Boolean | Type::Character => {
                        format!("\tmov byte ptr [rbp-{}], al", var.offset)
                    }
                }
                .into_boxed_str(),
            );
        }
        Statement::SingleInstruction(Instruction::Write(tokens)) => {
            let mut buffer = String::new();
            let mut vars_to_write: Vec<Variable> = Vec::new();
            for token in tokens {
                match token {
                    Token::IntLiteral(x) => buffer.push_str(&x.to_string()),
                    Token::RealLiteral(x) => buffer.push_str(&x.to_string()),
                    Token::StringLiteral(literal, _) => buffer.push_str(&literal),
                    Token::Identifier(ident) => {
                        let var = variables.get(&ident).ok_or(GenerationError::IdentifierNotDeclared)?;
                        buffer.push_str(match var.var_type {
                            Type::Real => "%lf",
                            Type::Integer => "%ld",
                            Type::Character => "%c",
                            Type::CharacterChain => "%s",
                            Type::Boolean => "%s",
                        });
                        vars_to_write.push(var.clone());
                    }
                    Token::True => buffer.push_str("verdadeiro"),
                    Token::False => buffer.push_str("falso"),
                    _ => unreachable!(),
                }
            }
            buffer.push_str("\\n");
            let i = program_data.strings.len();
            program_data.strings.push(buffer.into_boxed_str());
            instructions.push(format!("\tlea rdi, [str{i}]").into_boxed_str());
            let mut leftover: Vec<Variable> = Vec::new();
            let mut fp_passed = 0usize;
            let mut normal_passed = 0usize;
            for var in vars_to_write {
                match var.var_type {
                    Type::Integer | Type::CharacterChain => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            instructions.push(format!("\tmov {}, qword ptr [rbp-{}]", REGISTERS[normal_passed], var.offset).into_boxed_str());
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Boolean => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            program_data.bool_str = true;
                            instructions.push(Box::from("\tlea r10, [verdadeiro]"));
                            instructions.push(format!("\tlea {}, [falso]", REGISTERS[normal_passed]).into_boxed_str());
                            instructions.push(Box::from("\txor rax, rax"));
                            instructions.push(format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                            instructions.push(Box::from("\tcmp rax, 0"));
                            instructions.push(format!("\tcmovne {}, r10", REGISTERS[normal_passed]).into_boxed_str());
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Real => {
                        if fp_passed < 7 {
                            instructions.push(format!("\tmovsd xmm{fp_passed}, qword ptr [rbp-{}]", var.offset).into_boxed_str());
                            fp_passed += 1;
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Character => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            instructions.push(format!("\tmov {}, byte ptr [rbp-{}]", REGISTERS_8[normal_passed], var.offset).into_boxed_str());
                        } else {
                            leftover.push(var);
                        }
                    }
                }
            }
            leftover.reverse();
            for var in leftover {
                match var.var_type {
                    Type::Integer | Type::Real | Type::CharacterChain => {
                        instructions.push(format!("mov rax, qword ptr [rbp-{}]", var.offset).into_boxed_str());
                        instructions.push(Box::from("\tpush rax"));
                    }
                    Type::Boolean => {
                        program_data.bool_str = true;
                        instructions.push(Box::from("\tlea r10, [verdadeiro]"));
                        instructions.push(format!("\tlea r11, [falso]").into_boxed_str());
                        instructions.push(Box::from("\txor rax, rax"));
                        instructions.push(format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                        instructions.push(Box::from("\tcmp rax, 0"));
                        instructions.push(format!("\tcmovne r11, r10").into_boxed_str());
                        instructions.push(Box::from("\tpush r11"));
                    }
                    Type::Character => {
                        instructions.push(format!("mov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                        instructions.push(Box::from("\tpush al"));
                    }
                }
            }
            instructions.push(Box::from("\txor rax, rax"));
            instructions.push(Box::from("\tcall printf"));
        }
        _ => (),
    }
    Ok(())
}

struct ProgramData {
    strings: Vec<Box<str>>,
    bool_str: bool,
    pow: bool,
}

pub fn generate(program: Program) -> Result<Vec<Box<str>>, GenerationError> {
    let variables = program.variables;
    let mut program_data = ProgramData {
        strings: Vec::new(),
        bool_str: false,
        pow: false,
    };
    let mut instructions: Vec<Box<str>> = vec![
        Box::from(".global _start"),
        Box::from(".intel_syntax noprefix"),
        Box::from(".extern printf scanf"),
        Box::from("_start:"),
        Box::from("\tpush rbp"),
        Box::from("\tmov rbp, rsp"),
        format!("\tsub rsp, {}", program.stack_size).into_boxed_str(),
    ];

    for statement in program.statements {
        generate_statement(statement, &variables, &mut instructions, &mut program_data)?;
    }

    instructions.push(Box::from("\tleave"));
    instructions.push(Box::from("\tmov rax, 60"));
    instructions.push(Box::from("\txor rdi, rdi"));
    instructions.push(Box::from("\tsyscall"));
    for (i, string) in program_data.strings.into_iter().enumerate() {
        instructions.push(format!("str{i}:").into_boxed_str());
        instructions.push(format!("\t.asciz \"{}\"", string.replace("\"", "\\\"")).into_boxed_str());
    }
    if program_data.bool_str {
        instructions.push(Box::from("verdadeiro:"));
        instructions.push(Box::from("\t.asciz \"verdadeiro\""));
        instructions.push(Box::from("falso:"));
        instructions.push(Box::from("\t.asciz \"falso\""));
    }
    if program_data.pow {
        instructions.push(Box::from("pow:"));
        instructions.push(Box::from("\txor rax, rax"));
        instructions.push(Box::from("\tcmp rsi, 0"));
        instructions.push(Box::from("\tjl r_pow"));
        instructions.push(Box::from("\tmov rax, 1"));
        instructions.push(Box::from("l_pow:"));
        instructions.push(Box::from("\tcmp rsi, 0"));
        instructions.push(Box::from("\tje r_pow"));
        instructions.push(Box::from("\timul rax, rdi"));
        instructions.push(Box::from("\tdec rsi"));
        instructions.push(Box::from("\tjmp l_pow"));
        instructions.push(Box::from("r_pow:"));
        instructions.push(Box::from("\tret"));
    }
    Ok(instructions)
}