mod tokenizer;

fn main() {
    let mut args = std::env::args().skip(1);
    let mut output_file = String::from("a.out");
    let mut input_file: Option<String> = None;

    while let Some(arg) = args.next() {
        if arg.as_str() == "-o" {
            output_file = args.next().unwrap_or_else(|| {
                eprintln!("Erro: Falta o ficheiro de saída depois do -o.");
                std::process::exit(1);
            });
        } else if let Some(_) = input_file {
            eprintln!("Erro: O compilador só aceita um ficheiro de entrada.");
            std::process::exit(1);
        } else {
            input_file = Some(arg);
        }
    }

    let input_file = input_file.unwrap_or_else(|| {
        eprintln!("Erro: Faltam ficheiros de entrada.");
        std::process::exit(1);
    });

    let code = std::fs::read_to_string(input_file).unwrap_or_else(|err| {
        eprintln!("Erro I/O: {err}.");
        std::process::exit(1);
    });

    let tokens = tokenizer::tokenize(code).unwrap_or_else(|err| {
        eprintln!("{err}");
        std::process::exit(1);
    });

    dbg!(output_file, tokens);
}
