#[cfg(not(windows))]
mod generator;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let mut args = std::env::args().skip(1);
    let mut output_file: Option<String> = None;
    let mut input_file: Option<String> = None;
    let mut interpret = false;
    let mut emit_asm = false;

    while let Some(arg) = args.next() {
        if arg.as_str() == "-o" {
            output_file = Some(args.next().unwrap_or_else(|| {
                eprintln!("Erro: Falta o ficheiro de saída depois do -o.");
                std::process::exit(1);
            }));
        } else if arg.as_str() == "-i" {
            interpret = true;
        } else if arg.as_str() == "-s" {
            emit_asm = true;
        } else if let Some(_) = input_file {
            eprintln!("Erro: O compilador só aceita um ficheiro de entrada.");
            std::process::exit(1);
        } else {
            input_file = Some(arg);
        }
    }

    if output_file.is_none() && !interpret && !emit_asm {
        eprintln!("Erro: Tens de escolher entre interpretar e compilar!");
        std::process::exit(1);
    }

    let input_file = input_file.unwrap_or_else(|| {
        eprintln!("Erro: Faltam ficheiros de entrada.");
        std::process::exit(1);
    });

    let code = std::fs::read_to_string(input_file).unwrap_or_else(|err| {
        eprintln!("Erro I/O: {err}.");
        std::process::exit(1);
    });

    let tokens = lexer::tokenize(code).unwrap_or_else(|err| {
        eprintln!("{err}");
        std::process::exit(1);
    });

    let program = parser::parse(tokens).unwrap_or_else(|err| {
        eprintln!("{err}");
        std::process::exit(1);
    });

    if interpret {
        interpreter::interpret(&program);
    }

    #[cfg(not(windows))]
    if let Some(output_file) = output_file {
        let instructions = generator::generate(program).unwrap_or_else(|err| {
            eprintln!("{err}");
            std::process::exit(1);
        });
        let mut asm = String::new();
        for instruction in instructions {
            asm.push_str(&instruction);
            asm.push('\n');
        }
        std::fs::write("temp.s", &asm).err().map(|err| {
            println!("{err}");
        });
        let as_stderr = std::process::Command::new("as")
            .arg("temp.s")
            .arg("-o")
            .arg("temp.o")
            .output()
            .unwrap_or_else(|err| {
                println!("{err}");
                std::process::exit(1);
            })
            .stderr;
        let gcc_stderr = std::process::Command::new("gcc")
            .arg("-Ofast")
            .arg("-nostartfiles")
            .arg("temp.o")
            .arg("-o")
            .arg(output_file)
            .arg("-no-pie")
            .output()
            .unwrap_or_else(|err| {
                println!("{err}");
                std::process::exit(1);
            })
            .stderr;
        if !emit_asm {
            let _ = std::fs::remove_file("temp.s");
        }
        let _ = std::fs::remove_file("temp.o");
        if !as_stderr.is_empty() {
            println!(
                "{:?}",
                String::from_utf8(as_stderr).unwrap_or(String::from("error"))
            );
            std::process::exit(1);
        }
        if !gcc_stderr.is_empty() {
            println!(
                "{:?}",
                String::from_utf8(gcc_stderr).unwrap_or(String::from("error"))
            );
            std::process::exit(1);
        }
    } else if emit_asm {
        let instructions = generator::generate(program).unwrap_or_else(|err| {
            eprintln!("{err}");
            std::process::exit(1);
        });
        let mut asm = String::new();
        for instruction in instructions {
            asm.push_str(&instruction);
            asm.push('\n');
        }
        std::fs::write("temp.s", &asm).err().map(|err| {
            println!("{err}");
        });
    }

    #[cfg(windows)]
    if output_file.is_some() {
        eprintln!("O compilador não funciona no Windows, só no Linux e no macOS.");
        std::process::exit(1);
    }

    #[cfg(windows)]
    if emit_asm {
        let instructions = generator::generate(program).unwrap_or_else(|err| {
            eprintln!("{err}");
            std::process::exit(1);
        });
        let mut asm = String::new();
        for instruction in instructions {
            asm.push_str(&instruction);
            asm.push('\n');
        }
        std::fs::write("temp.s", &asm).err().map(|err| {
            println!("{err}");
        });
    }
}
