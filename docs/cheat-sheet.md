# 📋 ArgParser Cheat Sheet

A lightweight, record-based command-line argument parser for Free Pascal. This guide provides quick code examples for common tasks.

## 🚀 Complete Basic Example

Here's a full example demonstrating the basic workflow: initialize, define, parse, and access.

```pascal
// File: examples/SimpleTool/SimpleTool.pas
program SimpleTool;

uses
  ArgParser;

var
  Parser: TArgParser;
  InputFile: string;
  Verbose: Boolean;

begin
  // 1. Initialize the parser
  Parser.Init;
  Parser.SetUsage('Usage: mytool [options] <input-file>');

  // 2. Define options
  Parser.AddString('i', 'input', 'Input file path', '', True);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose output');

  // 3. Parse arguments
  Parser.ParseCommandLine;

  // 4. Handle errors or help
  if Parser.HasError then
  begin
    WriteLn('Error: ', Parser.Error);
    Parser.ShowUsage; // Frees internal resources
    Exit;
  end;

  if Parser.GetBoolean('help') then
  begin
    Parser.ShowHelp; // Frees internal resources
    Exit;
  end;

  // 5. Access parsed values
  InputFile := Parser.GetString('input');
  Verbose := Parser.GetBoolean('verbose');

  WriteLn('Input File: ', InputFile);
  if Verbose then
    WriteLn('Verbose mode enabled.');
end.
```

## 🎯 Supported Argument Formats

ArgParser supports multiple flexible formats for specifying arguments:

### Long Options
```bash
# Traditional space-separated format
--file input.txt
--count 42
--verbose

# New equals format (reduces ambiguity)
--file=input.txt
--count=42
--verbose=true
```

### Short Options
```bash
# Space-separated format
-f input.txt
-c 42
-v

# Attached format (no space)
-finput.txt
-c42
# PowerShell compatibility: handles cases where it splits as `-finput` and `.txt`
```

### Boolean Options
```bash
# Flag format (sets to true)
--verbose
-v

# Explicit format (new feature)
--verbose=true
--verbose=false
--quiet=true
```

### Mixed Usage
You can mix different formats in the same command:
```bash
mytool --input=data.txt -v --count 100 --output=result.txt
```

### Examples: tokens and Args arrays

What `Parse(const Args: TStringDynArray)` receives (conceptually `ParamStr(1..ParamCount)`):

```text
Command: mytool --file=input.txt -v
Args    = ["--file=input.txt", "-v"]

Command: mytool --count 10 -o result.txt
Args    = ["--count", "10", "-o", "result.txt"]

Command: mytool -finput .txt  # PowerShell may split short string values
Args    = ["-finput", ".txt"]  # parser reattaches → value "input.txt"
```

## ⚙️ API Quick Reference

### 1. Initialization

Always initialize the parser before defining options. You can also set a custom usage banner.

```pascal
var
  Parser: TArgParser;
begin
  Parser.Init;
  Parser.SetUsage('Usage: myapp [options] <source> <destination>');
end;
```

### 2. Defining Options

Define the command-line options your application accepts.

#### String

```pascal
// Basic string option with a default value
Parser.AddString('o', 'output', 'Output file name', 'out.txt');

// A required string option (no default needed)
Parser.AddString('i', 'input', 'Input file name', '', True);
```

#### Integer

```pascal
// Integer option with a default value
Parser.AddInteger('p', 'port', 'Port number', 8080);

// Required integer option
Parser.AddInteger('c', 'count', 'Number of items', 0, True);
```

#### Float

```pascal
// Float option with a default value
Parser.AddFloat('t', 'threshold', 'Confidence threshold', 0.75);
```

#### Boolean

Boolean options are `False` by default and become `True` if the flag is present.

```pascal
// A simple boolean flag
Parser.AddBoolean('v', 'verbose', 'Enable verbose logging');

// A flag that is True by default (rare)
Parser.AddBoolean('q', 'quiet', 'Disable output', False);
```

#### Array

Array options collect multiple values for the same flag.

```pascal
// Example: --files a.txt --files b.txt
Parser.AddArray('f', 'files', 'Input files to process', True);

// With a default array
Parser.AddArray('I', 'include', 'Include paths', ['/usr/lib', '/lib']);
```

### 3. Parsing

Parse the command-line arguments after all options are defined.

```pascal
// Parse directly from ParamStr
Parser.ParseCommandLine;
```

### 4. Accessing Values

Retrieve parsed values using the long option name.

```pascal
var
  Port: Integer;
  OutputFile: string;
  IsVerbose: Boolean;
  IncludePaths: TStringDynArray;
begin
  OutputFile := Parser.GetString('output');
  Port := Parser.GetInteger('port');
  IsVerbose := Parser.GetBoolean('verbose');
  IncludePaths := Parser.GetArray('include');
end;
```

## 🔢 Positionals, NArgs and repeated options

Positionals are ordered arguments added with `AddPositional`. Use the optional `NArgs` parameter to control how many tokens the positional consumes. Use `NArgs = -1` for greedy consumption.

Example: two file positionals (one greedy)

```pascal
Parser.AddPositional('input', atString, 'Input files', '', True, -1); // greedy: consume remaining non-option tokens
Parser.ParseCommandLine;
files := Parser.GetAllString('input'); // or GetAllArray depending on how you define it
```

GetAll* helpers collect multiple occurrences of the same option. For example, if the user passes `--tag a --tag b`, use `GetAllString('tag')` or `GetAllArray('tag')` to retrieve both values in order.

Boolean options also support a negation form using `--no-<name>` to explicitly set a boolean to False. Example: `--no-verbose`.

### 5. Error Handling & Help

ArgParser provides built-in methods for error reporting and displaying help text.

#### Checking for Errors

After parsing, check the `HasError` property. If it's `True`, you can retrieve the message from the `Error` property.

```pascal
if Parser.HasError then
begin
  WriteLn(ErrOutput, 'Error: ', Parser.Error);
  Parser.ShowUsage; // Frees internal resources
  Halt(1);
end;
```

#### Displaying Help and Usage

The library can automatically generate and display help text based on your defined options.

### Show Full Help (`--help`)

The `ShowHelp` procedure displays the complete, formatted help message and frees resources. You should exit the program after showing help.

```pascal
// Define a help flag (usually done with other options)
Parser.AddBoolean('h', 'help', 'Show this help message');

// After parsing, check if the flag was used
if Parser.GetBoolean('help') then
begin
  Parser.ShowHelp; // Displays help and frees resources
  Halt(0);
end;
```

### Show Compact Usage

The `ShowUsage` procedure displays only the compact, one-line usage banner. This is useful for showing a quick reminder when an error occurs.

```pascal
if Parser.HasError then
begin
  WriteLn(ErrOutput, 'Error: ', Parser.Error);
  Parser.ShowUsage; // e.g., "Usage: myapp [options] <input-file>" and frees resources
  Halt(1);
end;
```

### Set Custom Usage Banner

You can customize the usage banner shown by `ShowHelp` and `ShowUsage`. This is best set during initialization.

```pascal
Parser.Init;
Parser.SetUsage('Usage: myapp [options] <source> <destination>');
```
