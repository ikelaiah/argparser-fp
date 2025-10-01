# Changelog

All notable changes to this project are documented below. This file follows the "Keep a Changelog" format and the project follows Semantic Versioning.

For details on the project and examples, see the repository README and the `docs/` folder.

## [0.7.0] - 2025-10-01

Changed

- **Code Simplification**: Refactored internal implementation to reduce complexity and remove duplicate code patterns without losing any functionality.
- **Helper Functions**: Introduced helper functions for common operations like TArgValue initialization and array management to reduce repetitive code.
- **Cleaner Type Parsing**: Split the complex `ParseValue` method into focused type-specific parsing functions for better maintainability.
- **Simplified Tokenizer**: Extracted helper functions in ArgTokenizer to improve code readability and reduce complexity.
- **Streamlined Methods**: Consolidated duplicate initialization patterns in `AddXXX` methods and simplified array operations throughout the codebase.

Fixed

- **Improved Error Messages**: Fixed misleading error messages for missing required positional arguments. Now correctly shows "Missing required positional argument: filename" instead of "Missing required option: --filename" for better user experience and consistency with CLI conventions.

Note: This release contains internal simplifications, refactoring, and improved error messaging. All public APIs remain unchanged and fully backward compatible.

## [0.6.1] - 2025-10-01

Changed

- **Internal Code Simplification**: Removed unused internal methods `IsHelpRequested()` and `NormalizeToken()` to reduce code complexity and improve maintainability (~80 lines of dead code removed).
- **Documentation Updates**: Updated internal code comments to better reflect the separation of concerns between ArgParser and ArgTokenizer.
- **Maintainability**: All changes are internal only - no public API changes, full backward compatibility maintained.

## [0.6.0] - 2025-10-01

Changed

- **Code Simplification**: Refactored `ArgParser.pas` to reduce code duplication between `ParseCommandLine` and `ParseCommandLineKnown` by extracting common `--` separator logic into a shared helper method `SplitArgsAroundSeparator`.
- **Code Cleanup**: Removed redundant `ParseKnown` wrapper method that only delegated to `ParseKnownArgs`.
- **ArgTokenizer Improvement**: Extracted PowerShell quirk handling (appending tokens starting with '.') into a reusable helper function `AppendDotTokenIfPresent` to eliminate code duplication.
- **Maintainability**: No functional changes - all existing behavior preserved while making the codebase cleaner and easier to maintain.

## [0.5.0] - 2025-09-29

Added

- `ArgTokenizer.pas`: Added explanatory comments to `TokenizeArgs` describing the combined-short splitting heuristic (small all-alpha groups <= 3 are split) and the PowerShell dot-join quirk where a following token beginning with '.' is reattached for short-inline values.
- `ArgParser.pas`: Introduced a per-parser configuration field `FSplitCombinedShorts` (initialized from the legacy module-global `SplitCombinedShorts`) so callers can control combined-short splitting on a per-instance basis.
- `ArgParser.pas`: Added `SetAllowMultiple(const LongOpt: string; const Value: Boolean)` helper to enable accumulation behavior for repeated options.
- Tests: Added and updated tokenizer and parser unit tests covering combined-shorts, short-inline values, PowerShell '.' reattachment, single `-` and negative-number handling, `--name=value` splitting, `--` separator leftovers, and accumulation of repeated options.

Changed

- `ArgParser.pas`: Calls to the tokenizer temporarily set the module-global `SplitCombinedShorts` from the parser instance field and restore it afterwards to preserve backward compatibility while enabling per-instance control.

Notes

Default behavior is unchanged: combined-short splitting remains enabled by default. Callers who
need conservative behavior can set `FSplitCombinedShorts` to `False` on their `TArgParser` instance.
Documentation: help output is documented to show that `ShowHelp` appends " (required)" to any
option or positional marked as required.

## [0.4.0] - 2025-09-28

Added / Changed

- Extracted tokenization into a new `ArgTokenizer` unit and introduced focused unit tests for token shapes. This makes normalization rules (e.g., `--name=value`, positional marking, and raw-token preservation) easier to maintain and test.
- `Parse` now consumes token objects produced by `TokenizeArgs`, reducing complexity in `ArgParser.pas` and improving separation of concerns.
- Added `SplitCombinedShorts` configuration to `ArgTokenizer.pas` to control how short groups like `-abc` are handled (split into separate flags vs inline remainder).
- Tests added for tokenizer edge-cases (cohorts, short-inline values, PowerShell '.'-append quirk).

Notes

- This release is primarily a refactor and test coverage improvement; behavior is compatible with previous releases except where noted.

## [0.3.0] - 2025-09-27

Added / Changed

- Fast option lookup (`FLookup`) to map `-x` / `--name` switches to internal option records. This improves parsing performance and centralizes option registration.
- Positional arguments are strictly positional by default: `AddPositional` arguments are matched only by their position in the argument list and are no longer automatically registered as `--name` switches. This reduces ambiguity and better matches common CLI expectations.
- Documentation updates across `README.md`, `docs/cheat-sheet.md`, and the beginner's guide to explain the lookup behavior and the new positional semantics.

Fixed

- Fixed a small bug in `GetAllArray` and simplified `AddPositional` position index logic for maintainability.

Notes

- These changes are backward-compatible for most common use cases. Advanced callers that relied on positional arguments being accessible via `--name` should update their call sites to add an explicit option if they still need the named form.


## [0.2.0] - 2025-09-26

Added

- Positional arguments support via `AddPositional` with ordered matching and `NArgs` for fixed or variable consumption (`NArgs = -1`).
- `ParseCommandLineKnown(out Leftovers)` helper which supports the `--` separator and returns leftover tokens for forwarding to subcommands.
- Allow multiple occurrences for options (accumulation) and `GetAll*` accessors to retrieve all values in order (e.g., `GetAllString`, `GetAllArray`).
- Boolean negation using `--no-<option>` to explicitly set boolean flags to false.

Changed / Fixed

- `ParseCommandLine` now detects a `--` separator and leftovers are available via the `Leftovers` property.
- `SetError` no longer frees resources automatically; this preserves the error message so callers can print it before calling `ShowUsage`.
- Parsing no longer auto-displays help; callers should check the `help` flag after parsing and call `ShowHelp`/`ShowUsage` and exit. This avoids surprising resource finalization during parsing.
- Fixed shell-related short-inline value handling (PowerShell dot-join cases where `-finput.txt` could be split into `-finput` and `.txt`).
- Fixed memory leaks on error paths: `ShowUsage` and `ShowHelp` now free internal resources after printing usage/help.


## [0.1.0] - 2025-09-19

Initial public release (split out from TidyKit).

Added

- Examples and documentation
- Core features:
  - Define short (`-f`) and long (`--file`) options
  - Parse string, integer, float, boolean, and array value types
  - Support for `--name=value` format for all option types
  - Enhanced boolean options supporting `--verbose=true/false` syntax
  - Automatic generation of usage and help text
  - Callback support for immediate action on parsed arguments
  - Methods to retrieve parsed values by option name
- Comprehensive unit tests
- `README.md` with quick start guide and API reference
- Example applications demonstrating usage
- MIT License

Changed

- Project renamed from ParseArgs to ArgParser; documentation updated accordingly.

