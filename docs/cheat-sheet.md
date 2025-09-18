# 📋 ArgParser Cheat Sheet

A lightweight, record-based command-line argument parser for Free Pascal. This guide provides quick code examples for common tasks.

## 🚀 Complete Basic Example

Here’s a full example demonstrating the basic workflow: initialize, define, parse, and access.

```pascal
program MyTool;

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
  Parser.Parse(ParamStrArray);

  // 4. Handle errors or help
  if Parser.HasError then
  begin
    WriteLn('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
  end;

  if Parser.GetBoolean('help') then
    Parser.ShowHelp; // Exits automatically

  // 5. Access parsed values
  InputFile := Parser.GetString('input');
  Verbose := Parser.GetBoolean('verbose');

  WriteLn('Input File: ', InputFile);
  if Verbose then
    WriteLn('Verbose mode enabled.');
end.
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
// Use FPC's ParamStrArray helper function
Parser.Parse(ParamStrArray);
```

### 4. Accessing Values

Retrieve parsed values using the long option name.

```pascal
var
  Port: Integer;
  OutputFile: string;
  IsVerbose: Boolean;
  IncludePaths: TArrayOfString;
begin
  OutputFile := Parser.GetString('output');
  Port := Parser.GetInteger('port');
  IsVerbose := Parser.GetBoolean('verbose');
  IncludePaths := Parser.GetArray('include');
end;
```


### 5. Error Handling & Help

ArgParser provides built-in methods for error reporting and displaying help text.

#### Checking for Errors

After parsing, check the `HasError` property. If it's `True`, you can retrieve the message from the `Error` property.

```pascal
if Parser.HasError then
begin
  WriteLn(ErrOutput, 'Error: ', Parser.Error);
  Halt(1);
end;
```

#### Displaying Help and Usage

The library can automatically generate and display help text based on your defined options.

**Show Full Help (`--help`)**

The `ShowHelp` procedure displays the complete, formatted help message and then exits the program. It's typically triggered by a dedicated `--help` flag.

```pascal
// Define a help flag (usually done with other options)
Parser.AddBoolean('h', 'help', 'Show this help message');

// After parsing, check if the flag was used
if Parser.GetBoolean('help') then
  Parser.ShowHelp; // Displays help and exits
```

**Show Compact Usage**

The `ShowUsage` procedure displays only the compact, one-line usage banner. This is useful for showing a quick reminder when an error occurs.

```pascal
if Parser.HasError then
begin
  WriteLn(ErrOutput, 'Error: ', Parser.Error);
  Parser.ShowUsage; // e.g., "Usage: myapp [options] <input-file>"
  Halt(1);
end;
```

**Set Custom Usage Banner**

You can customize the usage banner shown by `ShowHelp` and `ShowUsage`. This is best set during initialization.

```pascal
Parser.Init;
Parser.SetUsage('Usage: myapp [options] <source> <destination>');
```