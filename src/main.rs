// Example usage: cargo run tests/hello.dcf -t <scan, parse,>
// Additional packages: anyhow (error types), nom (parsing), clap (cli args)
mod utils;

mod ast;
mod scope;
mod symtable;
mod token;
mod cfg;
mod tac;
mod x86;

mod parse;
mod scan;
mod semcheck;
mod traverse;
mod buildcfg;
mod codegen;

fn get_writer(output: &Option<std::path::PathBuf>) -> Box<dyn std::io::Write> {
    match output {
        Some(path) => Box::new(std::fs::File::create(path.as_path()).unwrap()),
        None => Box::new(std::io::stdout()), // Writes to stdout by default
    }
}

fn main() {
    let args = utils::cli::parse();
    let input = std::fs::read_to_string(&args.input).expect("Filename is incorrect.");

    // Use writeln!(writer, "template string") to write to stdout ot file.
    // Automatically ties the writer to output file, if specified
    let mut writer = get_writer(&args.output);
    let filename = args.input.to_string_lossy().to_string();

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
            scan::scan(&input, &filename, &mut writer, args.debug);
        }
        utils::cli::CompilerAction::Parse => {
            parse::parse(&input, &filename, &mut writer, args.debug);
        }
        utils::cli::CompilerAction::Inter => {
            semcheck::semcheck(&input, &filename, &mut writer, args.debug);
        }
        utils::cli::CompilerAction::Assembly => {
            codegen::generate_assembly(&input, &filename, &mut writer, args.debug);
        }
    }
}
