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
    /// Returns a new BuildProfile constructed from the
    /// given args.
    fn new(matches: &clap::ArgMatches) -> BuildProfile {
        if matches.contains_id("release") {
            BuildProfile::Release
        } else {
            BuildProfile::Debug
        }
    }

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
}

/// Build arguments including path to the compressed
/// cpio archive we use as a "ramdisk
#[derive(Clone, Debug)]
struct BuildArgs {
    profile: BuildProfile,
    cpioz: String,
}

impl BuildArgs {
    /// Extracts the build profile type from the given matched
    /// arguments.  Debug is the default.
    fn new(matches: &clap::ArgMatches) -> BuildArgs {
        let profile = BuildProfile::new(matches);
        let cpioz = matches.get_one::<String>("cpioz").unwrap().to_string();
        BuildArgs { profile, cpioz }
    }
}

fn main() {
    let matches = parse_args();
    match matches.subcommand() {
        Some(("build", m)) => build(BuildArgs::new(m), m.contains_id("locked")),
        Some(("test", m)) => {
            test(BuildProfile::new(m), m.contains_id("locked"))
        }
        Some(("disasm", m)) => disasm(
            BuildArgs::new(m),
            m.contains_id("locked"),
            m.contains_id("source").then_some("-S"),
        ),
        Some(("expand", _m)) => expand(),
        Some(("clippy", m)) => clippy(m.contains_id("locked")),
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
                clap::arg!(--locked "Build locked to Cargo.lock"),
                clap::arg!(--release "Build optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Build debug version (default)")
                    .conflicts_with("release"),
                clap::arg!(--cpioz "Path to compressed CPIO archive")
                    .takes_value(true)
                    .required(true),
            ]),
        )
        .subcommand(
            clap::Command::new("test").about("Run unit tests").args(&[
                clap::arg!(--locked "Build or test locked to Cargo.lock"),
                clap::arg!(--release "Test optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Test debug version (default)")
                    .conflicts_with("release"),
            ]),
        )
        .subcommand(
            clap::Command::new("disasm").about("disassemble phbl").args(&[
                clap::arg!(--locked "Build locked to Cargo.lock"),
                clap::arg!(--release "Disassemble optimized version")
                    .conflicts_with("debug"),
                clap::arg!(--debug "Disassemble debug version (default)")
                    .conflicts_with("release"),
                clap::arg!(--source "Interleave source and assembler output"),
                clap::arg!(--cpioz "Path to compressed CPIO archive")
                    .takes_value(true)
                    .default_value("/dev/null"),
            ]),
        )
        .subcommand(clap::Command::new("expand").about("Expand macros"))
        .subcommand(
            clap::Command::new("clippy")
                .about("Run cargo clippy linter")
                .args(&[clap::arg!(--locked "Lint locked to Cargo.lock")]),
        )
        .subcommand(clap::Command::new("clean").about("cargo clean"))
        .get_matches()
}

/// Runs a cross-compiled build.
fn build(args: BuildArgs, with_locked: bool) {
    std::env::set_var("PHBL_PHASE1_COMPRESSED_CPIO_ARCHIVE_PATH", args.cpioz);
    let build_type = args.profile.build_type().unwrap_or("");
    let locked = with_locked.then_some("--locked").unwrap_or("");
    let target = target();
    let args = format!(
        "build {locked} {build_type} \
            -Z build-std=core,alloc \
            -Z build-std-features=compiler-builtins-mem \
            --target {target}.json"
    );
    cmd(cargo(), args.split_whitespace()).run().expect("build successful");
}

/// Runs tests.
fn test(profile: BuildProfile, with_locked: bool) {
    let build_type = profile.build_type().unwrap_or("");
    let locked = with_locked.then_some("--locked").unwrap_or("");
    let args = format!("test {locked} {build_type}");
    cmd(cargo(), args.split_whitespace()).run().expect("test successful");
}

/// Build and disassemble the phbl binary.
fn disasm(build_args: BuildArgs, with_locked: bool, flags: Option<&str>) {
    build(build_args.clone(), with_locked);
    let triple = target();
    let profile_dir = build_args.profile.dir().to_str().unwrap();
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
fn clippy(with_locked: bool) {
    let locked = with_locked.then_some("--locked").unwrap_or("");
    let args = format!("clippy {locked}");
    cmd(cargo(), args.split_whitespace()).run().expect("clippy successful");
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
