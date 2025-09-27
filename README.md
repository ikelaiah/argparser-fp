# 🌿 ArgParser-FP: A simple command-line argument parser for Free Pascal

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-blue.svg)](https://www.lazarus-ide.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Version](https://img.shields.io/badge/Version-1.0.0-blueviolet.svg)]()

A lightweight, record-based command-line argument parser for Free Pascal. `ArgParser-FP` is designed for small to medium console applications, offering a clean API to handle arguments with minimal setup.

## ✨ Features

- **Simple API:** Define options with single-line convenience methods.
- **Type-Safe Parsing:** Natively parse strings, integers, floats, booleans, and arrays.
- **Flexible Syntax:** Support for multiple argument formats:
  - Long options: `--file input.txt` or `--file=input.txt`
  - Short options: `-f input.txt` or `-finput.txt`
  - Boolean flags: `--verbose` or `--verbose=true/false`
- **Automatic Help Text:** Generates `--help` and usage text from your option definitions.
- **Callbacks:** Execute a procedure immediately when an option is parsed.
- **Required Options:** Enforce mandatory arguments.
- **Default Values:** Provide default values for optional arguments.
- **Zero Dependencies:** Uses only standard Free Pascal RTL units.


## 🚀 Quick Start

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
  Parser.ParseCommandLine;

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
$ fpc MyApp.pas
$ ./MyApp --help

**Output**

```bash
Usage: MyApp [options]

Options:
  -h, --help     Show this help message
  -c, --count    Set count value
  -v, --verbose  Enable verbose mode
  -f, --file     Specify a file path
  -t, --tags     Comma-separated list of tags
  -h, --help     Show this help message
```

Example Usage

```bash
# Traditional format
$ ./MyApp --file input.txt --count 10 --verbose

# New equals format
$ ./MyApp --file=input.txt --count=10 --verbose=true

# Mixed formats
$ ./MyApp --file=input.txt -c 10 --verbose
```

## ⚠️ Common Pitfalls

- **Standalone tokens without a preceding option**
  - A token not starting with `-` must be the value of the previous option. Otherwise it triggers `Invalid argument format: <token>`.

- **Boolean flags and values**
  - Presence sets to true: `--verbose` ⇒ true.
  - If you need an explicit value, use `--verbose=true|false` or `-v=false`. Writing `--verbose false` treats `false` as a separate token and will fail.

- **Short options aren’t chained**
  - `-abc` is parsed as `-a` with value `bc`, not three flags. Pass them separately (`-a -b -c`) or use long options.

- **PowerShell split quirk (Windows)**
  - PowerShell may split `-finput.txt` into two tokens: `-finput` and `.txt`. The parser reattaches `.txt` for string options so the value becomes `input.txt`.

- **Arrays are comma-separated**
  - Use a single token like `--list=a,b,c`. If values contain spaces, quote according to your shell.

## -- separator and ParseCommandLineKnown

ArgParser-FP provides a convenience method `ParseCommandLineKnown(out Leftovers)` which behaves like `ParseCommandLine` but also supports a `--` separator. Tokens after `--` are not parsed as options and are returned in `Leftovers` for the caller to handle.

Examples:

```pascal
var
  Parser: TArgParser;
  leftovers: TStringDynArray;
begin
  Parser.Init;
  try
    Parser.AddBoolean('v','verbose','Enable verbose');
    Parser.ParseCommandLineKnown(leftovers);
    // leftovers contains tokens after `--`, if any
  finally
    Parser.Done;
  end;
end;
```

Use `ParseCommandLine` when you don't need the `--` semantics and prefer the simplest call-site.

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

1.  Copy `src/ArgParser.pas` into your project folder.
2.  Add `ArgParser` to your program's `uses` clause.
3.  Compile with FPC:

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
