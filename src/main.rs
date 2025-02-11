// Example usage: cargo run tests/hello.decaf --target=scan
mod add;
mod utils;
// Additional packages: anyhow (error types), nom (parsing), clap (cli args)

// Heap allocated structures:
// - Box: unique pointer (heap)
// - rc: shared pointer

fn get_writer(output: &Option<std::path::PathBuf>) -> Box<dyn std::io::Write> {
    match output {
        Some(path) => Box::new(std::fs::File::create(path.as_path()).unwrap()),
        None => Box::new(std::io::stdout())
    }
}


fn main() {
    let args = utils::cli::parse();
    let input = std::fs::read_to_string(&args.input).expect("Filename is incorrect.");

    // Use writeln!(writer, "template string") to write to stdout ot file.
    let _writer = get_writer(&args.output);

    // Print debugging information
    if args.debug {
        eprintln!(
            "Filename: {:?}\nDebug: {:?}\nOptimizations: {:?}\nOutput File: {:?}\nTarget: {:?}",
            args.input, args.debug, args.opt, args.output, args.target
        );
    }

    // Perform a compiler action based on the specified target
    match args.target {
        utils::cli::CompilerAction::Default => {
            panic!("Invalid target");
        }
        utils::cli::CompilerAction::Scan => {
            // todo!("scan");
            println!("arguments are: {}", input);
            scan(&input);
        }
        utils::cli::CompilerAction::Parse => {
            todo!("parse");
        }
        utils::cli::CompilerAction::Inter => {
            todo!("inter");
        }
        utils::cli::CompilerAction::Assembly => {
            todo!("assembly");
        }
    }
}


/*
 Input: a Decaf source file String as input. 

 Effects: Outputs a sequence of tokens. 
*/
fn scan(_file: &String) -> Vec<u8>{
    // careful about comments!
    let output = Vec::new();
    println!("SCANNING");
    output
}


/* 
 Input: a sequence of tokens produced by the scanner.

 Effects:
 - Verifies that tokens conform to valid Decaf via the language specification
 - Outputs a syntax tree representation of the Decaf program
*/
fn _parse() {
    // use nom for parser
    // enum for AST
    println!("PARSING");
}