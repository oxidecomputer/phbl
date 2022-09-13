# Pico Host Boot Loader

`phbl` is the program run from the x86 reset vector that loads
and invokes the phase1 host operating system package, consisting
of the host kernel and phase1 cpio archive.

It is loaded from SPI flash by the PSP, and execution starts in
16-bit real mode.  It is responsible for:

* bringing the bootstrap core up into 64-bit long mode with
  paging enabled
* decompressing the phase1 cpio archive into physical memory
* locating the kernel executable ELF image inside the archive
* loading the binary image into physical memory mapped at its
  linked addresses,
* and finally invoking the kernel's ELF entry point

The ZLIB compressed phase1 archive is compiled into `phbl` as a
binary blob of bytes.

The implementation and steps that `phbl` takes are described in
detail in [rfd284][1].

## Building phbl

We use `cargo` and the [`xtask`][2] pattern for builds.  Note
that the compressed cpio archive containing the phase1 bootstrap
is used as part of the build process, and so one must build that
and compress it first before building `phbl`: the process for
building a phase1 archive is beyond the scope of this document;
refer to the [Helios][3] documentation for details.  But note
that the [pinprick][4] utility is suitable for compressing a
phase1 archive so that it is compatible with `phbl`.

Let us assume that the compressed cpio archive is in a file
called `phase1.cpio.z` and that the `$CPIOZ` environment
variable points to it.  Then we may build `phbl` with:

```
cargo xtask build --cpioz=$CPIOZ
```

This generates a "Debug" binary in the file
`target/x86_64-oxide-none-elf/debug/phbl`.

## Phbl development

Modifying `phbl` follows the typical development patterns of
most Rust programs, and we have several `cargo xtask` targets to
help with common tasks.  Typically, one might use:

* `cargo xtask test` to run unit tests
* `cargo xtask clippy` to run the linter
* `cargo xtask clean` to remove build artifacts and intermediate
  files.
* `cargo xtask expand` to expand macros
* `cargo xtask disasm` to build the phbl image and dump a
  disassembly listing of it

`cargo check` is fully supported for e.g. editor integration,
and formatting should be kept consistent via `cargo fmt`.

Most targets will also accept either a `--release` or `--debug`
argument to get either an optimized or debugging build; debug
is the default.  To build a release version for production, run:

```
cargo xtask build --cpioz=$CPIOZ --release
```

This will produce an optimized standalone binary named
`target/x86_64-oxide-none-elf/release/phbl`.  If one builds
a debug binary, as in the previous section, it will be named,
`target/x86_64-oxide-none-elf/debug/phbl`.

These binaries are suitable for use with the
[amd-host-image-builder][5] tool.  For example, to create an
image suitable for writing to flash on a gimlet from a debug
`phbl` binary, one may change to the `amd-host-image-builder`
repository and run:

```
cargo run -- \
    -B amd-firmware/GN/1.0.0.1 \
    -B amd-firmware/GN/1.0.0.6 \
    -c etc/milan-gimlet-b.efs.json5
    -r ${PHBL_REPO_ROOT}/target/x86_64-oxide-none-elf/debug/phbl \
    -o milan-gimlet-b-phbl.img
```

The resulting `milan-gimlet-b-phbl.img` is suitable for writing
into a gimlet's SPI ROM.

Changes are submitted and reviewed using the GitHub pull request
model.  Because `phbl` is load bearing, all changes must be
reviewed.  CI triggered by github actions ensures that tests
pass.

[1]: https://rfd.shared.oxide.computer/rfd/0284
[2]: https://github.com/matklad/cargo-xtask
[3]: https://github.com/oxidecomputer/helios
[4]: https://github.com/oxidecomputer/pinprick/
[5]: https://github.com/oxidecomputer/amd-host-image-builder/
