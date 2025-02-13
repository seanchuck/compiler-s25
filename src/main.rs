// Example usage: cargo run tests/inputs/hello.dcf -t <scan, parse,>
mod add;
mod utils;
mod scan;
mod parse;
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
            scan::scan(&input);
        }
        utils::cli::CompilerAction::Parse => {
            todo!("parse");
            // parse::parse(&input);
        }
        utils::cli::CompilerAction::Inter => {
            todo!("inter");
        }
        utils::cli::CompilerAction::Assembly => {
            todo!("assembly");
        }
    }
}