# 🌿 ArgParser-FP: A simple command-line argument parser for Free Pascal

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-blue.svg)](https://www.lazarus-ide.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Version](https://img.shields.io/badge/Version-0.4.0-blueviolet.svg)](https://github.com/ikelaiah/argparser-fp/releases)

A lightweight, record-based command-line argument parser for Free Pascal. `ArgParser-FP` is designed for small to medium console applications, offering a clean API to handle arguments with minimal setup.

> [!WARNING]
> This library is currently in active development. While it is functional and has been tested, some features may change in future releases. Please review the [Changelog](CHANGELOG.md) for the latest updates.

## ✨ Features

- **Type-Safe Parsing:** Natively parse strings, integers, floats, booleans, and arrays.
- **Flexible Syntax:** Support for multiple argument formats:
  - Short options: `-f input.txt` or `-finput.txt`
  - Boolean flags: `--verbose` or `--verbose=true/false`
- **Automatic Help Text:** Generates `--help` and usage text from your option definitions.
- **Required Options:** Enforce mandatory arguments.
- **Default Values:** Provide default values for optional arguments.



Here is a complete example of a simple application:
```pascal
// File: examples/MyApp/MyApp.pas
program MyApp;

uses
  SysUtils,
  ArgParser;

var
  Parser: TArgParser;
  i: integer;

begin
  // Initialize
  Parser.Init;
  Parser.SetUsage('MyApp [options]');

  // Add options using convenience methods
  Parser.AddInteger('c', 'count', 'Set count value', 5);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose mode');
  Parser.AddString('f', 'file', 'Specify a file path', '', True); // Required
  Parser.AddArray('t', 'tags', 'Comma-separated list of tags');   // Add array option
  Parser.AddBoolean('h', 'help', 'Show this help message');

  // Parse command line arguments with one call

  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Exit;
  end;

  // Show help if requested
  if Parser.GetBoolean('help') then
  begin
    Parser.ShowHelp;
    Exit;
  end;

  // Access parsed values
  Writeln('Count: ', Parser.GetInteger('count'));
  Writeln('File: ', Parser.GetString('file'));

  // Display array values if provided
  if Length(Parser.GetArray('tags')) > 0 then
  begin
    Write('Tags: ');
    for i := 0 to High(Parser.GetArray('tags')) do
    begin
      Write(Parser.GetArray('tags')[i]);
      if i < High(Parser.GetArray('tags')) then
        Write(', ');
    end;
    Writeln;
  end;

  if Parser.GetBoolean('verbose') then
    Writeln('Verbose mode is ON');

  Writeln('Done.');
end.
```

Compile and run the application with --help to see the auto-generated documentation:

```bash
fpc MyApp.pas
./MyApp --help
```

Output:

```text
Usage: MyApp [options]

Options:
  -h, --help     Show this help message
  -c, --count    Set count value
  -v, --verbose  Enable verbose mode
  -f, --file     Specify a file path
  -t, --tags     Comma-separated list of tags
```

Example Usage

```bash
# Traditional format
./MyApp --file input.txt --count 10 --verbose

# Equals format
./MyApp --file=input.txt --count=10 --verbose=true

# Mixed formats
./MyApp --file=input.txt -v --count 10
```

Additional notes: quote arguments containing spaces according to your shell's rules.

## ⚠️ Common Pitfalls

- **Standalone tokens without a preceding option**
  - A token not starting with `-` must be the value of the previous option. Otherwise it triggers `Invalid argument format: <token>`.

- **Boolean flags and values**
  - Presence sets to true: `--verbose` ⇒ true.
  - If you need an explicit value, use `--verbose=true|false` or `-v=false`. Writing `--verbose false` treats `false` as a separate token and will fail.

- **Short combined flags**
  - The tokenizer's default policy (controlled by `SplitCombinedShorts` in `src/ArgTokenizer.pas`) will split small all-alpha combined shorts like `-abc` into separate flags `-a -b -c`. Mixed or numeric groups (for example `-a1b`) are preserved as a single token. Set `SplitCombinedShorts := False` if you prefer conservative behavior.

- **PowerShell split quirk (Windows)**
  - PowerShell may split `-finput.txt` into two tokens: `-finput` and `.txt`. The parser reattaches `.txt` for string options so the value becomes `input.txt`.

- **Single dash and negative numbers**
  - A single dash token `-` is preserved as a positional token (commonly used to mean stdin). Numeric tokens that start with `-` such as `-1` are treated as positional values unless an option matching that token exists.

  Examples:

  - Read from stdin: mytool - input.txt   # '-' preserved as a positional (stdin)
  - Negative numeric positional: mytool process -1  # '-1' is a positional value, not an option

- **Arrays are comma-separated**
  - Use a single token like `--list=a,b,c`. If values contain spaces, quote according to your shell.

-- separator and leftovers

`ParseCommandLine` now detects a `--` separator automatically. Any tokens after a `--` are not parsed as options and are returned to the caller via the `Leftovers` property. This simplifies the common call-site: you can call `ParseCommandLine` and then inspect `Parser.Leftovers` to forward any remaining arguments.

Examples:

```pascal
var
  Parser: TArgParser;
  leftovers: TStringDynArray;
begin
  Parser.Init;
  try
    Parser.AddBoolean('v','verbose','Enable verbose');
    Parser.ParseCommandLine;
    leftovers := Parser.Leftovers; // tokens after `--`, if any
  finally
    Parser.Done;
  end;
end;
```

  If you previously used `ParseCommandLineKnown`, it continues to work as before; `ParseCommandLine` simply provides the same `--` semantics with a simpler call-site.

Important: the parser no longer prints help or exits during parsing. The presence of `-h`/`--help` sets the built-in `help` flag; after calling `ParseCommandLine` your program should check `Parser.HasError` and `Parser.GetBoolean('help')` and then call `Parser.ShowUsage` or `Parser.ShowHelp` and exit if appropriate. This prevents the parser from freeing internal resources unexpectedly while your program continues running.

Additional notes

- Boolean negation: long boolean options also support a `--no-<name>` form to explicitly set a boolean flag to False. Example: `--no-verbose`.
- Positional arguments: `AddPositional` creates ordered (positional) arguments. Use the optional `NArgs` parameter to control how many tokens a positional consumes. `NArgs = -1` means ‘‘greedy’’ (consume until the next option or the end of the line).
- AllowMultiple / GetAll*: For options that can appear multiple times (for example `--tag a --tag b`), use the `GetAll*` helpers (e.g., `GetAllString`, `GetAllArray`) to retrieve every occurrence in the order parsed.

For deeper details and examples, see the [Beginner's Guide](docs/ArgParser.md#9-parsing-rules-and-behavior) and the [Cheat Sheet](docs/cheat-sheet.md#examples-tokens-and-args-arrays).

## 📖 System Requirements

- **Compiler**: Free Pascal Compiler (FPC) 3.2.2+
- **Platforms**: Windows, Linux (cross-platform by design)
- **Dependencies**: None. Uses only standard FPC RTL units.

## 📦 Installation

1. Copy the `src/` files (at least `src/ArgParser.pas` and `src/ArgTokenizer.pas`) into your project folder or add the `src/` folder to your build path.
2. Add `ArgParser` to your program's `uses` clause.
3. Compile with FPC:

```bash
fpc MyProgram.pas
```

## 📚 API Reference

For detailed documentation on all available procedures and functions, please see the **[Beginner's Guide](docs/ArgParser.md)** and the **[Cheat Sheet](docs/cheat-sheet.md)**.

## 💬 Community & Support

- **Questions?** [Open a discussion](https://github.com/ikelaiah/argparser-fp/discussions)
- **Found a bug?** [Report an issue](https://github.com/ikelaiah/argparser-fp/issues)

## ✅ Testing

1. Open the `TestRunner.lpi` using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
cd tests
./TestRunner.exe -a --format=plain
```

Note: v0.4.0 introduced a dedicated `ArgTokenizer` unit and focused tokenizer unit tests (`tests/ArgTokenizer.Test.pas`) to validate token shapes and normalization rules.

### What's new in v0.5.0

- A parser-level configuration flag `FSplitCombinedShorts` was added to `TArgParser`. It defaults to the previous global behavior but can be set per parser instance to control whether small all-alpha short groups like `-abc` are split into separate flags. This avoids surprising global state changes across modules.
- A small helper `SetAllowMultiple(const LongOpt: string; const Value: Boolean)` was added to enable accumulation (allow multiple occurrences) for a given long option at runtime.
- Tests were expanded to cover tokenizer edge cases (PowerShell dot-join, single dash, negative numbers) and repeated-option accumulation.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (git checkout -b feature/AmazingFeature)
3. Commit your Changes (git commit -m 'Add some AmazingFeature')
4. Push to the Branch (git push origin feature/AmazingFeature)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.


## 🙏 Acknowledgments

- [DuckDB Team](https://duckdb.org/) for the amazing database engine.
- [Free Pascal Dev Team](https://www.freepascal.org/) for the Pascal compiler.
- [Lazarus IDE Team](https://www.lazarus-ide.org/) for such an amazing IDE.
- [rednose🇳🇱🇪🇺](https://discord.com/channels/570025060312547359/570025355717509147/1299342586464698368) of the [Unofficial Free Pascal discord server](https://discord.com/channels/570025060312547359/570091337173696513) for providing the initial DuckDB Pascal bindings via [Chet](https://discord.com/channels/570025060312547359/570025355717509147/1299342586464698368).
- The kind and helpful individuals on various online platforms such as:
  - [Unofficial Free Pascal discord server](https://discord.com/channels/570025060312547359/570091337173696513).
  - [Free Pascal & Lazarus forum](https://forum.lazarus.freepascal.org/index.php).
  - [Tweaking4All Delphi, Lazarus, Free Pascal forum](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/).
  - [Laz Planet - Blogspot](https://lazplanet.blogspot.com/) / [Laz Planet - GitLab](https://lazplanet.gitlab.io/).
  - [Delphi Basics](https://delphibasics.co.uk/index.html).
