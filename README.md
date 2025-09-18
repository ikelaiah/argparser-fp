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
- **Automatic Help Text:** Generates `--help` and usage text from your option definitions.
- **Callbacks:** Execute a procedure immediately when an option is parsed.
- **Required Options:** Enforce mandatory arguments.
- **Default Values:** Provide default values for optional arguments.
- **Zero Dependencies:** Uses only standard Free Pascal RTL units.

## 🚀 Quick Start

Here is a complete example of a simple application:

```pascal
// File: examples/Basic/MyApp.pas
program MyApp;

uses
  SysUtils,
  ArgParser;

var
  Parser: TArgParser;

begin
  // Initialize
  Parser.Init;
  Parser.SetUsage('MyApp [options]');

  // Add options using convenience methods
  Parser.AddInteger('c', 'count', 'Set count value', 5);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose mode');
  Parser.AddString('f', 'file', 'Specify a file path', '', True); // Required

  // Parse and handle errors
  Parser.Parse(ParamStrArray);
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
  end;

  // Show help if requested
  if Parser.GetBoolean('help') then
  begin
    Parser.ShowHelp;
    Halt(0);
  end;

  // Access parsed values
  Writeln('Count: ', Parser.GetInteger('count'));
  Writeln('File: ', Parser.GetString('file'));
  if Parser.GetBoolean('verbose') then
    Writeln('Verbose mode is ON');

  Writeln('Done.');
end.
```

Compile and run the application with --help to see the auto-generated documentation:

```bash
$ fpc MyApp.pas
$ ./MyApp --help
```

**Output**

```bash
Usage: MyApp [options]

Options:
  -c, --count <integer>   Set count value (Default: 5)
  -v, --verbose           Enable verbose mode
  -f, --file <string>     Specify a file path (Required)
  -h, --help              Show this help message
```


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
$ cd tests
$ ./TestRunner.exe -a --format=plain
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
- The kind and helpful individuals on various online platforms such as;
    - [Unofficial Free Pascal discord server](https://discord.com/channels/570025060312547359/570091337173696513).
    - [Free Pascal & Lazarus forum](https://forum.lazarus.freepascal.org/index.php).
    - [Tweaking4All Delphi, Lazarus, Free Pascal forum](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/).
    - [Laz Planet - Blogspot](https://lazplanet.blogspot.com/) / [Laz Planet - GitLab](https://lazplanet.gitlab.io/).
    - [Delphi Basics](https://www.delphibasics.co.uk/index.html).