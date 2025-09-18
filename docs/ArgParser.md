# ArgParser – Beginner’s Guide

A simple, record-based command-line argument parser for Free Pascal (FPC 3.2.2+).

---

## 1. Introduction

`ParseArgs` helps you:

- Define short (`-f`) and long (`--file`) options
- Parse values: string, integer, float, boolean, or array
- Attach callbacks to handle options
- Auto-generate usage and help text

Ideal for small to medium Pascal programs.

---

## 2. Installation

1. Copy `ParseArgs.pas` into your project folder.
2. Add to your program’s `uses` clause:

   ```pascal
   uses
     SysUtils,
     ParseArgs;
   ```

3. Compile with FPC:

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
  Items: array of string;
begin
  // Initialize or reset parser (clears options, errors, and previous results)
  Parser.Init;
  Parser.SetUsage('myapp [options]');
  Parser.AddString('f', 'file', 'Input file path', 'default.txt');
  Parser.AddInteger('c', 'count', 'Number of items', 5);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose mode');
  Parser.AddArray('l', 'list', 'Comma-separated list');
  Parser.Parse(ParamStrArray);
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
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
  Parser.Parse(ParamStrArray);
  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
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
  ParseArgs;

var
  Parser: TArgParser;
  InputFile, OutputDir: string;
  Threads: Integer;
  Verbose: Boolean;
  Tags: TArrayOfString;
  i: Integer;
begin
  Parser.Init;
  Parser.SetUsage('ComplexApp [options]');

  Parser.AddString('i', 'input', 'Path to input CSV file', '', True);
  Parser.AddString('o', 'output', 'Directory for output', './out');
  Parser.AddInteger('t', 'threads', 'Number of worker threads', 4);
  Parser.AddBoolean('v', 'verbose', 'Verbose logging');
  Parser.AddArray('T', 'tags', 'Filter tags to process');

  Parser.Parse(ParamStrArray);

  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
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

## 7. API Reference

```pascal
TArgType = (atString, atInteger, atFloat, atBoolean, atArray);

TArgValue = record
  ArgType: TArgType;
  Str:     string;
  Int:     Integer;
  Flt:     Double;
  Bool:    Boolean;
  Arr:     array of string;
end;

TArgCallback      = procedure(const Value: TArgValue);
TArgCallbackClass = procedure(const Value: TArgValue) of object;

TArgParser = record
  procedure Init; // Initialize/reset parser state (clears options, errors, usage text, and results)
  procedure SetUsage(const AUsage: string);
  procedure Add(
    ShortOpt: Char;
    LongOpt: string;
    ArgType: TArgType;
    HelpText: string;
    Callback: TArgCallback;
    CallbackClass: TArgCallbackClass;
    Required: Boolean;
    DefaultValue: TArgValue
  );
  procedure AddString(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultValue: string);
  procedure AddInteger(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultValue: Integer; const Required: Boolean = False);
  procedure AddFloat(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultValue: Double = 0.0; const Required: Boolean = False);
  procedure AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultValue: Boolean = False; const Required: Boolean = False);
  procedure AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const Required: Boolean = False);
  procedure Parse(const Args: array of string);
  function HasError: Boolean;
  property Error: string read GetError;
  procedure ShowUsage;
  procedure ShowHelp;
  function OptionCount: Integer;
  function GetString(const LongOpt: string): string;
  function GetInteger(const LongOpt: string): Integer;
  function GetFloat(const LongOpt: string): Double;
  function GetBoolean(const LongOpt: string): Boolean;
  function GetArray(const LongOpt: string): array of string;
end;
```

---

## 8. Class/Record Diagram

```mermaid
classDiagram
    class TArgType {
        <<enumeration>>
        atString
        atInteger
        atFloat
        atBoolean
        atArray
    }
    class TArrayOfString {
        <<type>>
        array of string
    }
    class TArgValue {
        <<record>>
        ArgType: TArgType
        Str: string
        Int: Integer
        Flt: Double
        Bool: Boolean
        Arr: TArrayOfString
    }
    class TArgCallback {
        <<procedure>>
        +Invoke(Value: TArgValue)
    }
    class TArgCallbackClass {
        <<procedure of object>>
        +Invoke(Value: TArgValue)
    }
    class TArgOption {
        <<record>>
        ShortOpt: Char
        LongOpt: string
        ArgType: TArgType
        HelpText: string
        DefaultValue: TArgValue
        Callback: TArgCallback
        CallbackClass: TArgCallbackClass
        Required: Boolean
        WasSpecified: Boolean
    }
    class TParseResult {
        <<record>>
        Name: string
        Value: TArgValue
    }
    class TArgParser {
        <<record>>
        -FOptions: array of TArgOption
        -FResults: array of TParseResult
        -FUsage: string
        -FErrorMsg: string
        +Init()
        +SetUsage(AUsage: string)
        +Add(ShortOpt, LongOpt, ArgType, HelpText, Callback, CallbackClass, Required, DefaultValue)
        +Parse(Args: array of string)
        +HasError(): Boolean
        +GetError(): string
        +ShowUsage()
        +ShowHelp()
        +OptionCount(): Integer
        +GetString(LongOpt: string): string
        +GetInteger(LongOpt: string): Integer
        +GetFloat(LongOpt: string): Double
        +GetBoolean(LongOpt: string): Boolean
        +GetArray(LongOpt: string): TArrayOfString
        +AddString(ShortOpt, LongOpt, HelpText, DefaultValue, Required)
        +AddInteger(ShortOpt, LongOpt, HelpText, DefaultValue, Required)
        +AddFloat(ShortOpt, LongOpt, HelpText, DefaultValue, Required)
        +AddBoolean(ShortOpt, LongOpt, HelpText, DefaultValue, Required)
        +AddArray(ShortOpt, LongOpt, HelpText, Required)
    }
    TArgParser o-- "*" TArgOption : contains
    TArgParser o-- "*" TParseResult : stores
    TArgOption o-- "1" TArgValue : has
    TParseResult o-- "1" TArgValue : has
```

---

## 9. Tips & Best Practices

- Always initialize the `DefaultValue` record before `Add`.
- For boolean flags, presence ⇒ `True`.
- Set `Required := True` to enforce mandatory options.
- Choose method callbacks (`CallbackClass`) when updating object state.

Happy Free Pascal coding!
