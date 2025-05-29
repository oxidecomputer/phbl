// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//!
//! Build driver for pico host boot loader.
//!
use clap::Parser;
use duct::cmd;
use std::env;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[command(
    name = "phbl",
    author = "Oxide Computer Company",
    version = "0.1.0",
    about = "xtask build tool for pico host boot loader"
)]
struct Xtask {
    #[clap(subcommand)]
    cmd: Command,
}

#[derive(Parser)]
enum Command {
    /// Builds phbl
    Build {
        #[clap(flatten)]
        profile: BuildProfile,
        #[clap(flatten)]
        locked: Locked,
        #[clap(long)]
        target_dir: Option<PathBuf>,

        /// Path to compressed CPIO archive
        #[clap(long)]
        cpioz: PathBuf,
    },
    /// cargo clean
    Clean,
    /// Run cargo clippy linter
    Clippy {
        #[clap(flatten)]
        locked: Locked,
    },
    /// disassemble phbl
    Disasm {
        #[clap(flatten)]
        profile: BuildProfile,
        #[clap(flatten)]
        locked: Locked,

        /// Interleave source and assembler output
        #[clap(long)]
        source: bool,

        /// Path to compressed CPIO archive
        #[clap(long, default_value = "/dev/null")]
        cpioz: PathBuf,
    },
    /// Expand macros
    Expand,
    /// Run unit tests
    Test {
        #[clap(flatten)]
        profile: BuildProfile,
        #[clap(flatten)]
        locked: Locked,
    },
}

/// Mutually exclusive debug/release flags, used by all commands
/// that run builds.
#[derive(Clone, Parser)]
struct BuildProfile {
    /// Build debug version (default)
    #[clap(long, conflicts_with_all = &["release"])]
    debug: bool,

    /// Build optimized version
    #[clap(long)]
    release: bool,
}

impl BuildProfile {
    // Returns the cargo argument corresponding to the given
    // profile.
    fn to_str(&self) -> &'static str {
        self.release.then_some("--release").unwrap_or("")
    }

    // Returns the output subdirectory component corresponding
    // to the profile.
    fn dir(&self) -> &'static Path {
        Path::new(if self.release { "release" } else { "debug" })
    }
}

/// Cargo `--locked` setting; separate from BuildProfile because
/// `clippy` uses it but doesn't care about debug/release.
#[derive(Parser)]
struct Locked {
    /// Build locked to Cargo.lock
    #[clap(long)]
    locked: bool,
}

impl Locked {
    fn to_str(&self) -> &str {
        self.locked.then_some("--locked").unwrap_or("")
    }
}

fn main() {
    let xtask = Xtask::parse();
    match xtask.cmd {
        Command::Build { profile, target_dir, locked, cpioz } => {
            build(profile, target_dir, locked, cpioz)
        }
        Command::Test { profile, locked } => test(profile, locked),
        Command::Disasm { profile, locked, source, cpioz } => {
            disasm(profile, locked, source, cpioz)
        }
        Command::Expand => expand(),
        Command::Clippy { locked } => clippy(locked),
        Command::Clean => clean(),
    }
}

/// Runs a cross-compiled build.
fn build(
    profile: BuildProfile,
    target_dir: Option<PathBuf>,
    locked: Locked,
    cpioz: PathBuf,
) {
    std::env::set_var("PHBL_PHASE1_COMPRESSED_CPIO_ARCHIVE_PATH", cpioz);
    let profile = profile.to_str();
    let locked = locked.to_str();
    let target_dir = target_dir.unwrap_or("target".into());
    let target_dir = target_dir.display();
    let target = target();
    let args = format!(
        "build {profile} {locked} \
            -Z build-std=core,alloc \
            -Z build-std-features=compiler-builtins-mem \
            --target {target}.json \
            --target-dir {target_dir}"
    );
    cmd(cargo(), args.split_whitespace()).run().expect("build successful");
}

/// Runs tests.
fn test(profile: BuildProfile, locked: Locked) {
    let profile = profile.to_str();
    let locked = locked.to_str();
    let args = format!("test {profile} {locked}");
    cmd(cargo(), args.split_whitespace()).run().expect("test successful");
}

/// Build and disassemble the phbl binary.
fn disasm(profile: BuildProfile, locked: Locked, source: bool, cpioz: PathBuf) {
    let target_dir = None;
    build(profile.clone(), target_dir, locked, cpioz);
    let triple = target();
    let profile_dir = profile.dir().to_str().unwrap();
    let flags = source.then_some("-S").unwrap_or("");
    let args = format!("-Cd {flags} target/{triple}/{profile_dir}/phbl");
    println!("args = {args}");
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
fn clippy(locked: Locked) {
    let locked = locked.to_str();
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
    env_or("OBJDUMP", "llvm-objdump".into())
}
