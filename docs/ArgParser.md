# ArgParser – Beginner’s Guide

A simple, record-based command-line argument parser for Free Pascal (FPC 3.2.2+).

---

## 1. Introduction

`ArgParser` helps you:

- Define short (`-f`) and long (`--file`) options
- Parse values: string, integer, float, boolean, or array
- Attach callbacks to handle options
- Auto-generate usage and help text

Ideal for small to medium Pascal programs.

---

## 2. Installation

1. Copy `ArgParser.pas` into your project folder.
2. Add to your program’s `uses` clause:

   ```pascal
   uses
     SysUtils,
     ArgParser;
   ```

3. Compile with FPC:

## Tokenizer extraction (v0.4.0)

The parser's tokenization logic has been extracted into `ArgTokenizer.pas`.
This makes it easier to reason about token shapes such as `--name=value`, short
inline values (`-finput`), and combined short flags (`-abc`). The tokenizer
behavior is controlled by `SplitCombinedShorts` (in `src/ArgTokenizer.pas`,
default: True) which determines whether small all-alpha groups like `-abc` are
split into `-a -b -c` or treated as a short plus remainder (e.g. `-a` + `bc`).

To compile your program with FPC:

```bash
fpc MyProgram.pas
```

---

## 3. Quick Start

```pascal
var
  Parser: TArgParser;
  FilePath: string;
  Count: Integer;
  Verbose: Boolean;
  Items: TStringDynArray;
begin
  // Initialize or reset parser (clears options, errors, and previous results)
  Parser.Init;
  Parser.SetUsage('myapp [options]');
  Parser.AddString('f', 'file', 'Input file path', 'default.txt');
  Parser.AddInteger('c', 'count', 'Number of items', 5);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose mode');
  Parser.AddArray('l', 'list', 'Comma-separated list');
  Parser.ParseCommandLine;
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage; // Frees internal resources
    Exit;
  end;
  FilePath := Parser.GetString('file');
  Count := Parser.GetInteger('count');
  Verbose := Parser.GetBoolean('verbose');
  Items := Parser.GetArray('list');
  // Your program logic...
end.
```

---

## 4. Defining Options

With the new overloads you can define options in a single call:

```pascal
Parser.AddString('f', 'file', 'Input file path', 'default.txt');
Parser.AddInteger('c', 'count', 'Number of items', 10, True);
Parser.AddFloat('p', 'precision', 'Decimal precision', 3.14);
Parser.AddBoolean('v', 'verbose', 'Verbose mode');
Parser.AddArray('l', 'list', 'Comma-separated list');
```

These methods implicitly create the default `TArgValue` record. After parsing, retrieve values with:

```pascal
FilePath := Parser.GetString('file');
Count    := Parser.GetInteger('count');
Precision:= Parser.GetFloat('precision');
Verbose  := Parser.GetBoolean('verbose');
Items    := Parser.GetArray('list');
```

---

## 5. Example Program

```pascal
program MyApp;

uses
  SysUtils,
  ArgParser;

var
  Parser: TArgParser;
  V: TArgValue;

procedure OnCount(const Value: TArgValue);
begin
  Writeln('Count = ', Value.Int);
end;

procedure OnVerbose(const Value: TArgValue);
begin
  if Value.Bool then
    Writeln('Verbose mode ON');
end;

begin
  // Initialize
  Parser.Init;
  Parser.SetUsage('MyApp [options]');

  // --count, default 5
  V.ArgType := atInteger;
  V.Int := 5;
  Parser.Add('c','count', atInteger, 'Set count', nil, @OnCount, False, V);

  // --verbose flag
  V.ArgType := atBoolean;
  V.Bool := False;
  Parser.Add('v','verbose', atBoolean, 'Enable verbose', nil, @OnVerbose, False, V);

  // Parse and handle errors
  Parser.ParseCommandLine;
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage; // Frees internal resources
    Exit;
  end;

  Writeln('Done.');
end.
```

---

## 6. Complex Example

```pascal
program ComplexApp;

uses
  SysUtils,
  ArgParser;

var
  Parser: TArgParser;
  InputFile, OutputDir: string;
  Threads: Integer;
  Verbose: Boolean;
  Tags: TStringDynArray;
  i: Integer;
begin
  Parser.Init;
  Parser.SetUsage('ComplexApp [options]');

  Parser.AddString('i', 'input', 'Path to input CSV file', '', True);
  Parser.AddString('o', 'output', 'Directory for output', './out');
  Parser.AddInteger('t', 'threads', 'Number of worker threads', 4);
  Parser.AddBoolean('v', 'verbose', 'Verbose logging');
  Parser.AddArray('T', 'tags', 'Filter tags to process');

  Parser.ParseCommandLine;

  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage; // Frees internal resources
    Halt(1);
  end;

  InputFile := Parser.GetString('input');
  OutputDir := Parser.GetString('output');
  Threads   := Parser.GetInteger('threads');
  Verbose   := Parser.GetBoolean('verbose');
  Tags      := Parser.GetArray('tags');

  Writeln('Configuration:');
  Writeln('  Input  : ', InputFile);
  Writeln('  Output : ', OutputDir);
  Writeln('  Threads: ', Threads);
  Writeln('  Verbose: ', BoolToStr(Verbose, True));
  if Length(Tags) > 0 then
  begin
    Write('  Tags   :');
    for i := 0 to High(Tags) do
      Write(' ', Tags[i]);
    Writeln;
  end
  else
    Writeln('  Tags   : (none)');

  // ... perform processing ...
end.
```

---


## 7. Parsing rules and behavior

- **Help detection**
  - The parser no longer auto-displays help during parsing. A `-h` or `--help` token will set the built-in `help` flag; after calling `ParseCommandLine` your program should inspect `Parser.HasError` and `Parser.GetBoolean('help')` and then call `Parser.ShowUsage` or `Parser.ShowHelp` and exit if appropriate. This avoids surprising resource finalization during parsing.

- **Accepted option forms**
  - Long option with inline value: `--name=value`
  - Long option with next token value: `--name value`
  - Short option with inline value: `-n=value`
  - Short option with concatenated value: `-nvalue`
  - Short option with next token value: `-n value`

- **Boolean options**
  - Presence of a boolean option sets it to `True` (e.g., `--verbose` → true).
  - Inline values are accepted: `--verbose=true`, `--verbose=false`, `-v=false`.
  - If a boolean is present without a value, it becomes `True`.

- **String/number/array options**
  - If provided with `=value` or concatenated (e.g., `-fvalue`), that value is used.
  - Otherwise, if the next token does not start with `-`, it is taken as the value.
  - Arrays are parsed from a comma-separated string, whitespace-trimmed, empty parts ignored: `--list=a, b, c` → `["a","b","c"]`.

- **Required options**
  - After parsing all args, any option marked `Required=True` must appear at least once; otherwise, parsing fails with an error.
  - For required string options, an empty value is rejected.

Note: `ShowHelp` automatically appends " (required)" to the help text for any option
or positional argument whose `Required` field is `True`.

### Token array shape (what Parse receives)

`Parse(const Args: TStringDynArray)` expects a whitespace-delimited token array equivalent to `ParamStr(1..ParamCount)`. Each element is one token (switch or value).

- Example A

  Command:

  ```text
  myapp --name=alice -n42 --ratio 0.75 --items=a,b,c
  ```

  Args array:

  ```pascal
  ["--name=alice", "-n42", "--ratio", "0.75", "--items=a,b,c"]
  ```

- Example B (PowerShell split quirk for short string values)

  Command:

  ```text
  myapp -ooutput .txt
  ```

  Args array:

  ```pascal
  ["-ooutput", ".txt"]
  ```

  Interpretation: if `-o/--output` is a string option, the parser reattaches the following token when it starts with `.` → value becomes `output.txt`.

- **Unknowns and invalid formats**
  - Any token not starting with `-` is an error: `Invalid argument format: <token>`.
  - Any switch not matching a defined short/long option is an error: `Unknown option: <opt>`.
  - Type conversion failures produce `Invalid value for option <opt>` or for booleans `Invalid boolean value for option <opt>`.

- **Double-dash (`--`) and leftovers**
  - `ParseCommandLine` now supports a `--` separator. Tokens after `--` are not parsed as options and are returned via the `Leftovers` property so callers can forward them to subcommands or handle them specially.

### Tokenizer and compatibility (v0.4.0)

To make normalization logic easier to test and maintain, tokenization has been extracted into `ArgTokenizer.pas` (v0.4.0). The tokenizer produces a small token array that the parser consumes. Focused unit tests (`tests/ArgTokenizer.Test.pas`) validate common token shapes such as `--name=value`, positional tokens, and preservation of raw tokens for edge-case handling.

- **PowerShell compatibility**
  - Some shells (notably PowerShell) can split concatenated short string values such as `-finput.txt` into two tokens: `-finput` and `.txt`.
  - The parser detects this for string options and reattaches the following token if it begins with `.`. Example:
    - Input tokens: `-finput` `.txt` → parsed as `-f` with value `input.txt`.

- **Defaults and multiple occurrences**
  - If an option is not provided, its `DefaultValue` is used.
  - Options can appear multiple times; retrieval helpers like `GetString('name')` return the last provided value (most recent occurrence wins).

- **Positionals are strictly positional (v0.2.0)**
  - Positionals added with `AddPositional` are matched only by their order in the argument list and are not automatically registered as `--name` switches. This avoids ambiguity and matches common expectations. If you want a positional that is addressable by switch, add both an option and a positional, or use a future helper to opt-in the named form.

### Examples

```text
# Long with inline value
myapp --file=input.txt.gz

# Long with next token value
myapp --file input.txt.gz

# Short concatenated value
myapp -finput.txt.gz

# Short with next token value
myapp -f input.txt.gz

# Short combined boolean count with value
myapp -c2

# Boolean presence implies true
myapp --verbose

# Boolean with explicit false
myapp --verbose=false
```

## 8. Tips & Best Practices

- Always initialize the `DefaultValue` record before `Add`.
- For boolean flags, presence ⇒ `True`.
- Set `Required := True` to enforce mandatory options.
- Choose method callbacks (`CallbackClass`) when updating object state.
- Attached short options without spaces are supported: `-finput.txt`, `-c42`.
- PowerShell compatibility: if the shell splits `-finput.txt` as `-finput` and `.txt`, the parser reattaches `.txt` to the value.
- On error, print `Parser.Error`, call `Parser.ShowUsage`, then exit. No manual cleanup is needed.

### Common pitfalls

- Tokens not starting with `-` must be values for the preceding option
  - Standalone tokens like `input.txt` (not following an option expecting a value) cause `Invalid argument format: input.txt`.

- Boolean flags do not consume the next token as a value
  - Use `--flag=true` or `--flag=false` (or `-f=false`).
  - Writing `--flag false` will treat `false` as a separate token and fail.

- Required string options reject empty values
  - `--name=` is invalid if `name` is required.

- Short option chaining is not supported
  - `-abc` is interpreted as option `-a` with value `bc`, not three flags. Define and pass short options separately (e.g., `-a -b -c`) or use long options.

- PowerShell may split short string values (file extensions)
  - `-finput .txt` becomes two tokens; the parser reattaches `.txt` for string options to form `input.txt`.

- Arrays are comma-separated in a single token
  - Prefer `--list=a,b,c`. If you need spaces inside values, quote the entire argument per your shell rules.

## Why the parser uses a lookup map (technical rationale)

The parser now maintains a small lookup map (`FLookup`) that maps option keys (the exact short `'-x'` and long `'--name'` strings) to the internal option index. This change was made for several technical reasons:

- Performance: previously the parser scanned the options array linearly every time it needed to resolve a token (O(n) per token). A lookup map provides near-constant time resolution (amortized O(1)), which keeps parsing fast even as the number of options grows.

- Simpler normalization: when the parser has a reliable, cheap way to map a token to an option definition, the normalization and token-handling code can be simpler. For example, handling combined short-options with attached values (`-fvalue`) only needs to test `'-f'` in the lookup instead of re-scanning the whole options list.

- Single source of truth: the map is populated exactly when options are added (`AddOption`) so there is one canonical place to look up options. This reduces duplication and makes maintenance easier because option registration and lookup logic are centralized.

- Easier refactors and testing: with a dedicated lookup, future refactors (tokenizer extraction, different option forms, or a switch to a hash map) become localized changes. Unit tests can exercise lookup behavior in isolation.

Implementation notes:

- The map uses `TStringList` with `Objects[]` to store the option index. The keys are stored with their exact prefixes (`'-'` and `'--'`) so lookups match the normalized token forms used during parsing.

- The map is created in `Init` and freed in `Done`/finalization, and `AddOption` inserts both `-x` and `--long` entries when applicable.
  - The map is created in `Init` and freed in `Done`/finalization. Note: as of v0.2.0, `AddOption` only registers switch entries (`-x`/`--long`) for non-positional options. Positionals are not added to the switch lookup by default.

Overall, the lookup map is a small, low-risk change that improves performance and makes the parsing code easier to reason about and maintain.
