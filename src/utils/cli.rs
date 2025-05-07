/**
 * A generic command-line interface for 6.035 compilers.  This class
 * provides command-line parsing for student projects.  It recognizes
 * the required <tt>-target</tt>, <tt>-debug</tt>, <tt>-opt</tt>, and
 * <tt>-o</tt> switches, and generates a name for input and output
 * files.
 *
 * @author 6.1100 Staff, last updated January 2024
 */
use clap::Parser;
use std::collections::BTreeSet;

#[derive(Clone, clap::ValueEnum, Debug)]
pub enum CompilerAction {
    Default,
    Scan,
    Parse,
    Inter,
    Assembly,
}

#[derive(Clone, clap::ValueEnum, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Optimization {
    Cse,
    Cp, 
    Dce,
    All,
    Regalloc,
    Constprop,

    // "Negative" variants of optimizations
    #[clap(name = "-cse")]
    NoCse,
    #[clap(name = "-cp")]
    NoCp,
    #[clap(name = "-dce")]
    NoDce,
    #[clap(name = "-regalloc")]
    NoRegalloc,
    #[clap(name = "-constprop")]
    NoConstprop
}

#[derive(Parser, Debug)]
pub struct Args {
    /// compile to the given stage
    #[clap(short, long, value_enum, default_value_t=CompilerAction::Default, value_name = "stage")]
    pub target: CompilerAction,

    /// write output to
    #[clap(short, long, value_name = "outname")]
    pub output: Option<std::path::PathBuf>,

    /// Perform the listed optimizations
    #[clap(
        short = 'O',
        long,
        value_delimiter = ',',
        value_enum,
        value_name = "optimization,.."
    )]
    pub opt: Vec<Optimization>,

    /// Print debugging information
    #[arg(short, long, default_value_t = false)]
    pub debug: bool,

    /// Decaf file
    pub input: std::path::PathBuf,
}

impl Args {
    // Resolve the CLI arguments into a hashset of desired optimizations
    pub fn resolved_opts(&self) -> BTreeSet<Optimization> {
        use Optimization::*;

        let mut opts: BTreeSet<Optimization> = self.opt.iter().cloned().collect();

        // If 'all' is present, start with all
        if opts.contains(&All) {
            opts.remove(&All);
            opts.insert(Cse);
            opts.insert(Dce);
            opts.insert(Cp);
            opts.insert(Regalloc);
            opts.insert(Constprop);
        }

        // Remove disabled opts
        if opts.contains(&NoCse) {
            opts.remove(&Cse);
            opts.remove(&NoCse);
        }

        if opts.contains(&NoCp) {
            opts.remove(&Cp);
            opts.remove(&NoCp);
        }

        if opts.contains(&NoDce) {
            opts.remove(&Dce);
            opts.remove(&NoDce);
        }


        if opts.contains(&NoRegalloc) {
            opts.remove(&Regalloc);
            opts.remove(&NoRegalloc);
        }

        opts
    }
}


pub fn parse() -> Args {
    Args::parse()
}