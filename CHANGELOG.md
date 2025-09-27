# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-09-25

### Fixed

- Short option with inline value now works reliably across shells:
  - Handles `-finput.txt` and cases where PowerShell splits it into `-finput` and `.txt`.
- No memory leaks on error paths:
  - `ShowUsage` frees internal resources (and clears stored strings) so examples that print error + usage and exit have 0 unfreed blocks.
  - `ShowHelp` continues to free internal resources after printing full help.

### Changed

- `SetError` no longer frees resources automatically; this preserves the error message so callers can print it before calling `ShowUsage`.
- Documentation updated to reflect the above behavior and the recommended error-handling pattern.

### Added

- Positional arguments support via `AddPositional` with ordered matching and `NArgs` for fixed or greedy consumption (`NArgs = -1`).
- `ParseCommandLineKnown(out Leftovers)` helper which supports the `--` separator and returns leftover tokens for forwarding to subcommands.
- Allow multiple occurrences for options (accumulation) and `GetAll*` accessors to retrieve all values in order (e.g., `GetAllString`, `GetAllArray`).
- Boolean negation using `--no-<option>` to explicitly set boolean flags to false.


## [0.9.0] - 2025-09-19

First release as an independent package, previously was part of TidyKit.

### Added

- More examples
- Initial release of ArgParser-FP.
- Core features:
  - Define short (`-f`) and long (`--file`) options.
  - Parse string, integer, float, boolean, and array value types.
  - Support for `--name=value` format for all option types.
  - Enhanced boolean options supporting `--verbose=true/false` syntax.
  - Automatic generation of usage and help text.
  - Callback support for immediate action on parsed arguments.
  - Methods to retrieve parsed values by option name.
- Comprehensive unit tests.
- `README.md` with quick start guide and API reference.
- Example applications demonstrating usage.
- MIT License.

### Changed

- Changed name from ParseArgs to ArgParser.
- Updated all docs to reflect the changes