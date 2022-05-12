//!
//! Build driver for pico host boot loader.
//!
use clap;
use duct::cmd;
use std::env;
use std::path::{Path, PathBuf};
use std::process;

/// BuildProfile defines whether we build in release or
/// debug mode.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BuildProfile {
    Debug,
    Release,
}

impl BuildProfile {
    /// Returns the subdirectory component corresponding
    /// to the build type.
    fn dir(self) -> &'static Path {
        Path::new(match self {
            Self::Debug => "debug",
            Self::Release => "release",
        })
    }

    /// Yields the appropriate cargo argument for the given
    /// build profile.
    fn build_type(self) -> Option<&'static str> {
        match self {
            Self::Release => Some("--release"),
            Self::Debug => None,
        }
    }
    /// Extracts the build profile type from the given matched
    /// arguments.  Debug is the default.
    fn matched_type(matches: &clap::ArgMatches) -> BuildProfile {
        if matches.contains_id("release") {
            BuildProfile::Release
        } else {
            BuildProfile::Debug
        }
    }
}

fn main() {
    let matches = parse_args();
    match matches.subcommand() {
        Some(("build", m)) => build(BuildProfile::matched_type(m)),
        Some(("test", m)) => test(BuildProfile::matched_type(m)),
        Some(("disasm", m)) => disasm(
            BuildProfile::matched_type(m),
            m.contains_id("source").then_some("-S"),
        ),
        Some(("expand", _m)) => expand(),
        Some(("clippy", _m)) => clippy(),
        Some(("clean", _m)) => clean(),
        _ => {
            println!("Unknown command");
            process::exit(1);
        }
    }
}

/// Parse program arguments and return the match structure.
fn parse_args() -> clap::ArgMatches {
    clap::Command::new("xtask")
        .version("0.1.0")
        .author("Oxide Computer Company")
        .about("xtask build tool for pico host boot loader")
        .subcommand(
            clap::Command::new("build").about("Builds phbl").args(&[
                clap::arg!(--release "Build optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Build debug version (default)")
                    .conflicts_with("release"),
            ]),
        )
        .subcommand(
            clap::Command::new("test").about("Run unit tests").args(&[
                clap::arg!(--release "Test optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Test debug version (default)")
                    .conflicts_with("release"),
            ]),
        )
        .subcommand(
            clap::Command::new("disasm").about("disassemble phbl").args(&[
                clap::arg!(--release "Disassemble optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Disassemble debug version (default)")
                    .conflicts_with("release"),
                clap::arg!(--source "Interleave source and assembler output"),
            ]),
        )
        .subcommand(clap::Command::new("expand").about("Expand macros"))
        .subcommand(
            clap::Command::new("clippy").about("Run cargo clippy linter"),
        )
        .subcommand(clap::Command::new("clean").about("cargo clean"))
        .get_matches()
}

/// Runs a cross-compiled build.
fn build(profile: BuildProfile) {
    let build_type = profile.build_type().unwrap_or("");
    let target = target();
    let args = format!(
        "build {build_type} \
            -Z build-std=core,alloc \
            -Z build-std-features=compiler-builtins-mem \
            --target {target}.json"
    );
    cmd(cargo(), args.split_whitespace()).run().expect("build successful");
}

/// Runs tests.
fn test(profile: BuildProfile) {
    let c = if let Some(arg) = profile.build_type() {
        cmd!(cargo(), "test", arg)
    } else {
        cmd!(cargo(), "test")
    };
    c.run().expect("test successful");
}

/// Build and disassemble the phbl binary.
fn disasm(profile: BuildProfile, flags: Option<&str>) {
    build(profile);
    let triple = target();
    let profile_dir = profile.dir().to_str().unwrap();
    let flags = flags.unwrap_or("");
    let args = format!("-Cd {flags} target/{triple}/{profile_dir}/phbl");
    cmd(objdump(), args.split_whitespace())
        .run()
        .expect("disassembly successful");
}

/// Expands macros.
fn expand() {
    cmd!(cargo(), "rustc", "--", "-Zunpretty=expanded")
        .run()
        .expect("expand successful");
}

/// Runs the Clippy linter.
fn clippy() {
    cmd!(cargo(), "clippy").run().expect("clippy successful");
}

/// Runs clean on the project.
fn clean() {
    cmd!(cargo(), "clean").run().expect("clean successful");
}

/// Returns the value of the given environment variable,
/// or the default if unspecified.
fn env_or(var: &str, default: &str) -> String {
    env::var(var).unwrap_or(default.into())
}

/// Returns the name of the cargo binary.
fn cargo() -> String {
    env_or("CARGO", "cargo")
}

/// Returns the target triple we are building for.
fn target() -> String {
    env_or("TARGET", "x86_64-oxide-none-elf")
}

/// Locates the LLVM objdump binary.
fn objdump() -> String {
    env_or("OBJDUMP", &toolchain_binary("llvm-objdump"))
}

/// Attempts to locate a program shipped with the toolchain.
fn toolchain_binary(program: &str) -> String {
    let toolchain = env_or("RUSTUP_TOOLCHAIN", "x86_64-unknown-none");
    let pos = toolchain.find('-').map(|p| p + 1).unwrap_or(0);
    let host = toolchain[pos..].to_string();
    let home = env_or("RUSTUP_HOME", "");
    let mut path = PathBuf::from(home);
    path.push("toolchains");
    path.push(toolchain);
    path.push("lib");
    path.push("rustlib");
    path.push(host);
    path.push("bin");
    path.push(program);
    if path.exists() {
        path.into_os_string().into_string().unwrap()
    } else {
        program.into()
    }
}
