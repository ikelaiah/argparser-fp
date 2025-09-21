# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [1.0.0] - 2025-09-19

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