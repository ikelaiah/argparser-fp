//-------------------------------------------------------------------------------
// Unit: ArgParser
//
// A lightweight, record-based command-line argument parser for Free Pascal.
// Supports string, integer, float, boolean, and array types.
// Provides methods to define options, parse command-line arguments, and retrieve parsed values.
//
// Boolean Options:
// - Present means true: myapp --verbose → verbose = true
// - Explicit values: myapp --verbose=false → verbose = false
// - Negation form: myapp --no-verbose → verbose = false (for "verbose" option)
//
// Note: The help printer (ShowHelp) automatically appends " (required)" to the
// help text for any option or positional argument marked Required = True.
//
//===============================================================================
//  ARGUMENT PARSING ARCHITECTURE OVERVIEW
//
//  The argument parsing process follows a structured pipeline:
//
//  1. TOKENIZATION (ArgTokenizer.pas)
//     Raw Args: ["--file=test.txt", "-v", "input.dat"]
//     ↓
//     Tokens: [
//       {Kind: tkOption, OptName: "--file", HasValue: true, ValueStr: "test.txt"}
//       {Kind: tkOption, OptName: "-v", HasValue: false, ValueStr: ""}  
//       {Kind: tkPositional, OptName: "", HasValue: false, ValueStr: "input.dat"}
//     ]
//
//  2. OPTION LOOKUP & VALIDATION
//     For each token:
//     ↓
//     Token OptName → Option Definition Index → TArgOption record
//     ↓
//     Validate: option exists, type matches, required fields present
//
//  3. TYPE PARSING & CONVERSION
//     ValueStr → Typed Value based on ArgType
//     ↓
//     "test.txt" (atString)  → TArgValue{Str: "test.txt"}
//     "123" (atInteger)      → TArgValue{Int: 123}
//     "true" (atBoolean)     → TArgValue{Bool: true}
//     "a,b,c" (atArray)      → TArgValue{Arr: ["a","b","c"]}
//
//  4. RESULT STORAGE & CALLBACKS
//     Parsed values stored in FResults array for later retrieval
//     Optional callbacks executed immediately upon successful parsing
//
//  5. ACCESS & RETRIEVAL
//     GetString("file") → "test.txt"
//     GetBoolean("verbose") → true
//     GetArray("tags") → ["tag1", "tag2", "tag3"]
//
//  SPECIAL FEATURES:
//  • Boolean Negation: Use --no-<option> to set boolean options to false
//    Example: --no-verbose sets a 'verbose' boolean option to false
//  • Greedy Collection: Options/positionals with NArgs=-1 consume multiple tokens
//    and join them with commas for array parsing or concatenated string values
//  • Separator Support: Use "--" to separate options from trailing arguments
//    Everything after "--" goes to leftovers, not parsed as options
//
//===============================================================================
//-------------------------------------------------------------------------------
unit ArgParser;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Types, StrUtils, ArgTokenizer;

{ TArgType: Enumeration of supported argument types. }
type
  TArgType = (atString, atInteger, atFloat, atBoolean, atArray);
  
  { TArgValue: Container for storing a parsed argument value. }
  TArgValue = record
    ArgType: TArgType;   { The type of this value }
    Str: string;         { Value when ArgType = atString }
    Int: Integer;        { Value when ArgType = atInteger }
    Flt: Double;         { Value when ArgType = atFloat }
    Bool: Boolean;       { Value when ArgType = atBoolean }
    Arr: TStringDynArray; { Value when ArgType = atArray }
  end;

  { TArgCallback: Procedure type for callbacks on parsed values. }
  TArgCallback = procedure(const Value: TArgValue);
  { TArgCallbackClass: Class procedure type for callbacks on parsed values. }
  TArgCallbackClass = procedure(const Value: TArgValue) of object;

  { TArgOption: Record for defining a command-line option. }
  TArgOption = record
    ShortOpt: Char;      { Short option switch (e.g., '-x') }
    LongOpt: string;     { Long option switch (e.g., '--example') }
    ArgType: TArgType;   { Type of value expected for this option }
    HelpText: string;    { Brief description of the option }
    DefaultValue: TArgValue; { Default value if not provided }
    Callback: TArgCallback; { Callback procedure for this option }
    CallbackClass: TArgCallbackClass; { Class callback procedure for this option }
    Required: Boolean;   { Flag indicating if this option is required }
    { Extended metadata }
    IsPositional: Boolean; { True for positional arguments }
    PositionIndex: Integer; { Order index for positionals (0-based) }
    AllowMultiple: Boolean; { Allow repeated occurrences (accumulate) }
    NArgs: Integer;        { Number of tokens to consume: 0 = single token (default), -1 = greedy (consume all remaining until next option, joined with commas). Note: >0 values are accepted but currently behave as 0 }
  end;

  { TOptionsArray: Dynamic array of TArgOption records. }
  TOptionsArray = array of TArgOption;

  { TParseResult: Record for storing a parsed result. }
  TParseResult = record
    Name: string;        { Name of the option }
    Value: TArgValue;    { Parsed value }
  end;
  { TParseResults: Dynamic array of TParseResult records. }
  TParseResults = array of TParseResult;

  { Additional dynamic array types used by GetAll* accessors }
  TBooleanDynArray = array of Boolean;
  TStringDynArrayArray = array of TStringDynArray;

  { TArgParser: Main record to define options, parse arguments, and access results. }
  TArgParser = record
  private
    FOptions: TOptionsArray;  { Defined command-line options }
    FUsage: string;           { Custom usage banner text }
    FError: string;           { Error message if parsing fails }
    FHasError: Boolean;       { Flag indicating parsing error }
    FResults: TParseResults;  { Parsed results }
    FLeftovers: TStringDynArray; { Unknown/unconsumed tokens when using ParseKnown }
    FParseKnown: Boolean; { If true, unknown tokens are returned in Leftovers instead of error }
    FLookup: TStringList; { Maps '-x' and '--long' to option index (as string); case-insensitive for fast lookup }
    FSplitCombinedShorts: Boolean; { Per-parser override for tokenizer combined-short splitting }

    { Locate option by input switch. }
    function FindOption(const Opt: string): Integer;
    { Convert string to TArgValue based on ArgType. }
    function ParseValue(const ValueStr: string; const ArgType: TArgType; var Value: TArgValue): Boolean;
    { Type-specific parsing helpers }
    function ParseStringValue(const ValueStr: string; var Value: TArgValue): Boolean;
    function ParseIntegerValue(const ValueStr: string; var Value: TArgValue): Boolean;
    function ParseFloatValue(const ValueStr: string; var Value: TArgValue): Boolean;
    function ParseBooleanValue(const ValueStr: string; var Value: TArgValue): Boolean;
    function ParseArrayValue(const ValueStr: string; var Value: TArgValue): Boolean;
    { Internal helper to add an option record. }
    procedure AddOption(const Option: TArgOption);
    { Record a parsing error. }
    procedure SetError(const AError: string);
    { Helpers to make Parse more readable }
    { Clears error flags/messages before a new Parse run }
    procedure ResetParseState;
    { Finalizes and clears FResults from any previous Parse run }
    procedure ClearPreviousResults;
    { Populates typed Value for the option; may consume next token; enforces required/empty rules and type parsing }
  function ParseOptionValue(const OptionIdx: Integer; var HasValue: Boolean; var ValueStr: string; const Tokens: TArgTokenArray; var tIdx: Integer; var Value: TArgValue; const CurrentOpt: string): Boolean;
    { Invokes any registered callbacks for this option }
    procedure RunCallbacks(const OptionIdx: Integer; const Value: TArgValue);
    { Appends the parsed (Name, Value) pair to FResults }
    procedure AppendResult(const OptionIdx: Integer; const Value: TArgValue);
  procedure AppendLeftover(const Token: string);
    { Split args around '--' separator into left and right parts }
    procedure SplitArgsAroundSeparator(const Args: TStringDynArray; out LeftArgs, RightArgs: TStringDynArray);
    { Ensures all options marked Required=True were provided at least once }
    function CheckRequiredOptionsPresent: Boolean;
    { Parse the provided command-line arguments array. }
  procedure Parse(const Args: TStringDynArray);
  public
    { Initialize parser state. Call before adding any options. }
    procedure Init;
    { Add a new option with all parameters including callbacks. }
    procedure Add(const ShortOpt: Char; const LongOpt: string; const ArgType: TArgType; const HelpText: string; const Callback: TArgCallback; const CallbackClass: TArgCallbackClass; const Required: Boolean; const DefaultValue: TArgValue);
    { Parse command-line arguments directly from ParamStr (program's actual command-line).
      Handles "--" separator: everything after "--" goes to Leftovers. }
    procedure ParseCommandLine;
  { Parse command-line with support for `--` separator and unknown options.
    Unknown options and tokens after "--" are returned in Leftovers instead of causing errors.
    Useful for wrapper programs that pass remaining args to another program. }
  procedure ParseCommandLineKnown(out Leftovers: TStringDynArray);
    { Enable or disable AllowMultiple for a named long option. Returns silently if not found. }
    procedure SetAllowMultiple(const LongOpt: string; const Value: Boolean);
    { Returns True if an error occurred during parsing. }
    function HasError: Boolean;
    { Read-only property to get error message. }
    property Error: string read FError;
    { Print formatted help information based on defined options. }
    procedure ShowHelp;
    { Print usage banner and options. }
    procedure ShowUsage;
    { Set custom usage banner (e.g., program name and synopsis). }
    procedure SetUsage(const AUsage: string);
    { Get count of options defined. }
    function OptionCount: Integer;
    { Convenience overloads to add typed options more easily. }
    procedure AddString(const ShortOpt: Char; const LongOpt, HelpText: string; const Default: string = ''; const Required: Boolean = False);
    procedure AddInteger(const ShortOpt: Char; const LongOpt, HelpText: string; const Default: Integer = 0; const Required: Boolean = False);
    procedure AddFloat(const ShortOpt: Char; const LongOpt, HelpText: string; const Default: Double = 0.0; const Required: Boolean = False);
    procedure AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string; const Default: Boolean = False; const Required: Boolean = False);
    procedure AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const Required: Boolean = False); overload;
    procedure AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultArr: array of string; const Required: Boolean = False); overload;
  { Add a positional argument (ordered). PositionIndex may be omitted (append).
    Parameters:
      - Name: logical name used to retrieve the value(s)
      - ArgType: type of the positional (atString, atInteger, atFloat, atBoolean, atArray)
      - NArgs: number of tokens to consume: 0 = single token (default), -1 = greedy (consume all remaining non-option tokens, joined with commas). Note: >0 values are accepted but currently behave as 0
    
    Positionals are matched in the order they are added. Greedy positionals (NArgs=-1)
    will consume all remaining non-option tokens and join them with commas.
    
    Example:
      parser.AddPositional('input', atString, 'Input file');
      parser.AddPositional('output', atString, 'Output file');
      Command: myapp input.txt output.txt → input='input.txt', output='output.txt'
  }
  procedure AddPositional(const Name: string; const ArgType: TArgType; const HelpText: string; const Default: string = ''; const Required: Boolean = False; const NArgs: Integer = 0);
  { Return all occurrences for options (accumulated).
    Use these helpers when an option may appear multiple times (AllowMultiple) or when
    working with array-like options. Values are returned in the order parsed.
    
    Example:
      parser.SetAllowMultiple('verbose', True);
      Command: myapp -v -v -v → GetAllBoolean('verbose') returns [true, true, true]
  }
  function GetAllString(const LongOpt: string): TStringDynArray;
  function GetAllInteger(const LongOpt: string): TIntegerDynArray;
  function GetAllFloat(const LongOpt: string): TDoubleDynArray;
  function GetAllBoolean(const LongOpt: string): TBooleanDynArray;
  function GetAllArray(const LongOpt: string): TStringDynArrayArray;
  { Parse known args: returns leftovers (unknown tokens) instead of failing }
  procedure ParseKnownArgs(const Args: TStringDynArray; out Leftovers: TStringDynArray);
  { Return leftovers captured from the last ParseCommandLine call }
  function GetLeftovers: TStringDynArray;
  property Leftovers: TStringDynArray read GetLeftovers;
    { Accessors for parsed values by long option name.
      Note: These return the MOST RECENT value if an option appears multiple times.
      Use GetAll* methods to retrieve all occurrences. }
    function GetString(const LongOpt: string): string;
    function GetInteger(const LongOpt: string): Integer;
    function GetFloat(const LongOpt: string): Double;
    function GetBoolean(const LongOpt: string): Boolean;
    function GetArray(const LongOpt: string): TStringDynArray;
    procedure Done;
    class operator Finalize(var r: TArgParser);
  end;

implementation

{===============================================================================
  Utility Functions for Array and Value Management
  
  These helper functions provide clean abstractions for common operations
  needed throughout the parsing process.
===============================================================================}

{ Convert ParamStr array to dynamic string array
  
  Free Pascal's ParamStr function provides access to command-line arguments,
  but we need them in a dynamic array format for easier processing.
  
  ParamStr Layout:
    ParamStr(0) = Program name (not included in result)
    ParamStr(1) = First argument
    ParamStr(2) = Second argument
    ...
    ParamStr(ParamCount) = Last argument
}
function ParamStrToArray: TStringDynArray;
var
  i: Integer;
begin
  Result := nil; // Explicitly initialize to avoid warning
  SetLength(Result, ParamCount);
  for i := 1 to ParamCount do
    Result[i-1] := ParamStr(i);
end;

{ TArgValue Factory Function
  
  Creates a properly initialized TArgValue record with all fields set to
  safe default values. This prevents memory issues and ensures consistent
  initialization across the codebase.
  
  TArgValue Memory Layout:
  ┌─────────┬─────────┬─────────┬─────────┬─────────┬─────────────┐
  │ ArgType │   Str   │   Int   │   Flt   │  Bool   │     Arr     │
  │(variant)│ string  │ Integer │ Double  │ Boolean │StringDynArr │
  └─────────┴─────────┴─────────┴─────────┴─────────┴─────────────┘
  
  Only the field corresponding to ArgType should be used, but all fields
  are initialized for safety and to prevent memory corruption.
}
function CreateArgValue(const ArgType: TArgType): TArgValue;
begin
  Finalize(Result);           // Clean any existing managed memory
  Result.ArgType := ArgType;  // Set the discriminator field
  Result.Str := '';           // Initialize string field
  Result.Int := 0;            // Initialize integer field  
  Result.Flt := 0.0;          // Initialize float field
  Result.Bool := False;       // Initialize boolean field
  Result.Arr := nil;          // Initialize array field
end;

{ Dynamic Array Append Helper
  
  Free Pascal doesn't provide built-in dynamic array append functionality,
  so we implement it manually. This function safely extends an array and
  adds a new element at the end.
  
  Array Growth Strategy:
    Current: [item1, item2, item3]
    Append:  "item4"
    Result:  [item1, item2, item3, item4]
}
procedure AppendToArray(var Arr: TStringDynArray; const Value: string);
begin
  SetLength(Arr, Length(Arr) + 1);  // Extend array by one element
  Arr[High(Arr)] := Value;          // Set the new element value
end;

{ TArgParser }

procedure TArgParser.Init;
var
  i: Integer;
begin
  for i := 0 to High(FOptions) do
    Finalize(FOptions[i]);
  FOptions := nil;
  for i := 0 to High(FResults) do
    Finalize(FResults[i].Value);
  FResults := nil;
  FError := '';
  FUsage := '';
  AddBoolean('h', 'help', 'Show this help message');
  FParseKnown := False;
  // Default behavior copies module-level setting to allow backwards compatibility
  FSplitCombinedShorts := SplitCombinedShorts;
  // Initialize lookup mapping
  FLookup := TStringList.Create;
  FLookup.CaseSensitive := False;
end;

procedure TArgParser.Add(const ShortOpt: Char; const LongOpt: string;
  const ArgType: TArgType; const HelpText: string;
  const Callback: TArgCallback;
  const CallbackClass: TArgCallbackClass;
  const Required: Boolean;
  const DefaultValue: TArgValue);
var
  Option: TArgOption;
begin
  Option.ShortOpt := ShortOpt;
  Option.LongOpt := LongOpt;
  Option.ArgType := ArgType;
  Option.HelpText := HelpText;
  Option.Callback := Callback;
  Option.CallbackClass := CallbackClass;
  Option.Required := Required;
  Option.DefaultValue := DefaultValue;
  // Initialize extended metadata defaults
  Option.IsPositional := False;
  Option.PositionIndex := -1;
  Option.AllowMultiple := False;
  Option.NArgs := 0;
  AddOption(Option);
end;

function TArgParser.FindOption(const Opt: string): Integer;
var
  idx: Integer;
  s: string;
begin
  Result := -1;
  if not Assigned(FLookup) then
    Exit;
  // Normalize key to match how we stored them in AddOption
  s := Opt;
  idx := FLookup.IndexOf(s);
  if idx >= 0 then
    Result := Integer(PtrInt(FLookup.Objects[idx]));
end;

{===============================================================================
  TYPE-SPECIFIC VALUE PARSING FUNCTIONS
  
  These functions handle the conversion of string values to typed TArgValue
  records. Each function is responsible for one specific data type and
  implements appropriate validation and conversion logic.

  PARSING STRATEGY OVERVIEW:
  
  Input: ValueStr (string) + Target ArgType
       ↓
  ┌─────────────────────────────────────────────────────────────────────────┐
  │                      TYPE PARSING DISPATCH                             │
  ├─────────────────────────────────────────────────────────────────────────┤
  │                                                                         │
  │  ParseValue() → case ArgType of                                         │
  │                   atString  → ParseStringValue()                        │
  │                   atInteger → ParseIntegerValue()                       │
  │                   atFloat   → ParseFloatValue()                         │
  │                   atBoolean → ParseBooleanValue()                       │
  │                   atArray   → ParseArrayValue()                         │
  │                                                                         │
  └─────────────────────────────────────────────────────────────────────────┘
       ↓
  Output: TArgValue with appropriate field set + Boolean success flag

  VALIDATION EXAMPLES:
  
  String:   Any input is valid → Result.Str := ValueStr
  Integer:  "123" → 123, "abc" → Error
  Float:    "3.14" → 3.14, "not_a_number" → Error  
  Boolean:  "true"/"false"/"1"/"0" → true/false, "maybe" → Error
  Array:    "a,b,c" → ["a","b","c"], "" → []

===============================================================================}

function TArgParser.ParseValue(const ValueStr: string; const ArgType: TArgType; var Value: TArgValue): Boolean;
begin
  // Initialize value with proper defaults for the target type
  Value := CreateArgValue(ArgType);
  
  // Dispatch to type-specific parsing function
  case ArgType of
    atString:  Result := ParseStringValue(ValueStr, Value);
    atInteger: Result := ParseIntegerValue(ValueStr, Value);
    atFloat:   Result := ParseFloatValue(ValueStr, Value);
    atBoolean: Result := ParseBooleanValue(ValueStr, Value);
    atArray:   Result := ParseArrayValue(ValueStr, Value);
  else
    Result := False; // Unknown type - should never happen
  end;
end;

{ String Value Parser
  
  Strings are the most permissive type - any input is considered valid.
  No conversion or validation is needed, just direct assignment.
  
  Examples:
    "hello"     → "hello"
    ""          → "" (empty string)
    "123"       → "123" (stored as string, not number)
    "true"      → "true" (stored as string, not boolean)
}
function TArgParser.ParseStringValue(const ValueStr: string; var Value: TArgValue): Boolean;
begin
  Value.Str := ValueStr;  // Direct assignment - all strings are valid
  Result := True;         // String parsing never fails
end;

{ Integer Value Parser
  
  Attempts to convert string input to integer using Free Pascal's built-in
  TryStrToInt function, which handles various integer formats safely.
  
  Valid Formats:
    "123"     → 123
    "-456"    → -456
    "0"       → 0
    "+789"    → 789
  
  Invalid Formats:
    "abc"     → Parse error
    "12.34"   → Parse error (use float type)
    "12abc"   → Parse error
    ""        → Parse error
}
function TArgParser.ParseIntegerValue(const ValueStr: string; var Value: TArgValue): Boolean;
begin
  Result := TryStrToInt(ValueStr, Value.Int);
end;

{ Float Value Parser
  
  Attempts to convert string input to floating-point number using Free Pascal's
  TryStrToFloat function, which respects locale-specific decimal separators.
  
  Valid Formats:
    "3.14"    → 3.14
    "-2.5"    → -2.5
    "123"     → 123.0
    "0.0"     → 0.0
    "1e5"     → 100000.0 (scientific notation)
  
  Invalid Formats:
    "abc"     → Parse error
    "3.14.15" → Parse error
    ""        → Parse error
}
function TArgParser.ParseFloatValue(const ValueStr: string; var Value: TArgValue): Boolean;
begin
  Result := TryStrToFloat(ValueStr, Value.Flt);
end;

{ Boolean Value Parser
  
  Converts string input to boolean using Free Pascal's TryStrToBool function,
  which recognizes several standard boolean representations.
  
  True Values:
    "true", "True", "TRUE"
    "yes", "Yes", "YES"  
    "1"
    "on", "On", "ON"
  
  False Values:
    "false", "False", "FALSE"
    "no", "No", "NO"
    "0"
    "off", "Off", "OFF"
  
  Invalid Formats:
    "maybe"   → Parse error
    "1.0"     → Parse error
    ""        → Parse error
}
function TArgParser.ParseBooleanValue(const ValueStr: string; var Value: TArgValue): Boolean;
begin
  Result := TryStrToBool(ValueStr, Value.Bool);
end;

{ Array Value Parser
  
  Parses comma-separated values into a string array. Empty segments are
  filtered out, and each segment is trimmed of whitespace.
  
  Array Parsing Algorithm:
  
  Input: "item1, item2 ,item3,  ,item4"
       ↓
  1. Split on comma: ["item1", " item2 ", "item3", "  ", "item4"]
       ↓
  2. Trim whitespace: ["item1", "item2", "item3", "", "item4"]  
       ↓
  3. Filter empty: ["item1", "item2", "item3", "item4"]
       ↓
  Output: Value.Arr := ["item1", "item2", "item3", "item4"]
  
  Examples:
    "a,b,c"       → ["a", "b", "c"]
    "x, y , z"    → ["x", "y", "z"] (whitespace trimmed)
    "single"      → ["single"]
    ""            → [] (empty array)
    "a,,b"        → ["a", "b"] (empty segments filtered)
}
function TArgParser.ParseArrayValue(const ValueStr: string; var Value: TArgValue): Boolean;
var
  Parts: TStringDynArray;
  i: Integer;
  s: string;
begin
  // Split input string on comma delimiter
  Parts := SplitString(ValueStr, ',');
  
  // Process each segment: trim whitespace and filter empties
  for i := 0 to High(Parts) do
  begin
    s := Trim(Parts[i]);  // Remove leading/trailing whitespace
    if s <> '' then       // Skip empty segments
      AppendToArray(Value.Arr, s);
  end;
  
  Result := True;  // Array parsing never fails (worst case: empty array)
end;

procedure TArgParser.ResetParseState;
begin
  { Clear error status and message. Called at the beginning of Parse(). }
  FHasError := False;
  FError := '';
end;

procedure TArgParser.ClearPreviousResults;
var
  k: Integer;
begin
  { Finalize any previously stored results to avoid memory leaks across runs. }
  for k := 0 to High(FResults) do
    Finalize(FResults[k].Value);
  SetLength(FResults, 0);
end;

function TArgParser.ParseOptionValue(const OptionIdx: Integer; var HasValue: Boolean; var ValueStr: string; const Tokens: TArgTokenArray; var tIdx: Integer; var Value: TArgValue; const CurrentOpt: string): Boolean;
begin
  { Based on option type and HasValue/ValueStr, populate Value.
    - Booleans: presence implies True unless an explicit value is provided.
      Special case: --no-<option> sets boolean options to False.
    - Non-booleans: use inline value if present; otherwise consume next token if it isn't an option.
    - Required string options: reject empty values.
    - Perform type conversion using ParseValue and surface clear errors.
  }
  Result := False;

  // Boolean options: present means true unless explicit value provided
  if FOptions[OptionIdx].ArgType = atBoolean then
  begin
    // Support --no-<long> negation form (e.g., --no-verbose for a "verbose" option)
    if (Pos('--no-', CurrentOpt) = 1) and (SameText(Copy(CurrentOpt, 6, MaxInt), FOptions[OptionIdx].LongOpt)) then
    begin
      Value.Bool := False;
      HasValue := True;
      Result := True;
      Exit;
    end;

    if HasValue then
    begin
      if not ParseValue(ValueStr, atBoolean, Value) then
      begin
        SetError('Invalid boolean value for option ' + CurrentOpt);
        Exit;
      end;
    end
    else
    begin
      Value.Bool := True;
    end;
    HasValue := True;
    Result := True;
    Exit;
  end;

  // Non-boolean options
  if HasValue then
  begin
    if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType = atString) and (ValueStr = '') then
    begin
      SetError('Empty value not allowed for required option: ' + CurrentOpt);
      Exit;
    end;
    if not ParseValue(ValueStr, FOptions[OptionIdx].ArgType, Value) then
    begin
      SetError('Invalid value for option ' + CurrentOpt);
      Exit;
    end;
  end
  else if (tIdx < High(Tokens)) and (Tokens[tIdx+1].Kind = tkPositional) then
  begin
    ValueStr := Tokens[tIdx+1].ValueStr;
    if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType = atString) and (ValueStr = '') then
    begin
      SetError('Empty value not allowed for required option: ' + CurrentOpt);
      Exit;
    end;
    if not ParseValue(ValueStr, FOptions[OptionIdx].ArgType, Value) then
    begin
      SetError('Invalid value for option ' + CurrentOpt);
      Exit;
    end;
    HasValue := True;
    Inc(tIdx);
  end;

  // Required non-boolean missing value
  if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType <> atBoolean) and not HasValue then
  begin
    SetError('Missing value for required option: ' + CurrentOpt);
    Exit;
  end;

  Result := True;
end;

procedure TArgParser.RunCallbacks(const OptionIdx: Integer; const Value: TArgValue);
begin
  { Invoke any registered callbacks for this option. }
  if Assigned(FOptions[OptionIdx].Callback) then
    FOptions[OptionIdx].Callback(Value);
  if Assigned(FOptions[OptionIdx].CallbackClass) then
    FOptions[OptionIdx].CallbackClass(Value);
end;

procedure TArgParser.AppendResult(const OptionIdx: Integer; const Value: TArgValue);
begin
  { Append the parsed result (name = long option name) to FResults. }
  SetLength(FResults, Length(FResults) + 1);
  FResults[High(FResults)].Name := FOptions[OptionIdx].LongOpt;
  FResults[High(FResults)].Value := Value;
end;

function TArgParser.CheckRequiredOptionsPresent: Boolean;
var
  j, k: Integer;
  FoundRequired: Boolean;
begin
  { After all args are processed, verify each Required option was provided. }
  Result := True;
  for j := 0 to High(FOptions) do
  begin
    if FOptions[j].Required then
    begin
      FoundRequired := False;
      for k := 0 to High(FResults) do
      begin
        if FResults[k].Name = FOptions[j].LongOpt then
        begin
          FoundRequired := True;
          Break;
        end;
      end;
      if not FoundRequired then
      begin
        if FOptions[j].IsPositional then
          SetError('Missing required positional argument: ' + FOptions[j].LongOpt)
        else
          SetError('Missing required option: --' + FOptions[j].LongOpt);
        Result := False;
        Exit;
      end;
    end;
  end;
end;

{===============================================================================
  MAIN PARSING ALGORITHM
  
  The Parse method implements the core argument parsing logic. It processes
  tokenized command-line arguments and matches them against defined options,
  handling type conversion, validation, and result storage.

  PARSING PIPELINE OVERVIEW:
  
  Input: Tokenized arguments from ArgTokenizer
       ↓
  ┌─────────────────────────────────────────────────────────────────────────┐
  │                        PARSING PIPELINE                                 │
  ├─────────────────────────────────────────────────────────────────────────┤
  │ 1. INITIALIZATION                                                       │
  │    • Reset error state                                                  │
  │    • Clear previous results                                             │
  │    • Build positional argument order list                               │
  │    • Tokenize input arguments                                           │
  │                                                                         │
  │ 2. TOKEN PROCESSING LOOP                                                │
  │    For each token:                                                      │
  │    ┌─────────────────┐                                                  │
  │    │ Token Analysis  │                                                  │
  │    └─────────────────┘                                                  │
  │           │                                                             │
  │           ▼                                                             │
  │    ┌─────────────────┐     ┌─────────────────┐                          │
  │    │ Positional?     │────>│ Match Position  │                          │
  │    └─────────────────┘     └─────────────────┘                          │
  │           │                                                             │
  │           ▼                                                             │
  │    ┌─────────────────┐     ┌─────────────────┐                          │
  │    │ Option Token?   │────>│ Lookup Option   │                          │
  │    └─────────────────┘     └─────────────────┘                          │
  │           │                         │                                   │
  │           ▼                         ▼                                   │
  │    ┌─────────────────┐     ┌─────────────────┐                          │
  │    │ Parse Value     │     │ Type Convert    │                          │
  │    └─────────────────┘     └─────────────────┘                          │
  │           │                         │                                   │
  │           ▼                         ▼                                   │
  │    ┌─────────────────┐     ┌─────────────────┐                          │
  │    │ Run Callbacks   │     │ Store Result    │                          │
  │    └─────────────────┘     └─────────────────┘                          │
  │                                                                         │
  │ 3. VALIDATION                                                           │
  │    • Check all required options were provided                           │
  │    • Report first missing required option if any                        │
  │                                                                         │
  │ 4. CLEANUP                                                              │
  │    • Finalize temporary values                                          │
  │    • Set error state if validation failed                               │
  └─────────────────────────────────────────────────────────────────────────┘

  OPTION MATCHING ALGORITHM:
  
  Token OptName → FLookup Map → Option Index → FOptions[Index]
  
  Example:
    Token: {OptName: "--verbose"}
         ↓
    FLookup["--verbose"] → Index 3
         ↓  
    FOptions[3] → {ShortOpt: 'v', LongOpt: "verbose", ArgType: atBoolean}

  VALUE PARSING FLOW:
  
  Input Token → Extract ValueStr → Type-Specific Parser → Validated TArgValue
  
  Examples:
    "--count=5"     → "5"        → ParseIntegerValue → {Int: 5}
    "-v"            → ""         → ParseBooleanValue → {Bool: true}  
    "--file=a.txt"  → "a.txt"    → ParseStringValue  → {Str: "a.txt"}
    "--tags=a,b,c"  → "a,b,c"    → ParseArrayValue   → {Arr: ["a","b","c"]}

  ERROR HANDLING STRATEGY:
  
  Any parsing error immediately:
  1. Calls SetError() with descriptive message
  2. Sets Result := False and exits
  3. Preserves error state for caller inspection
  4. Ensures proper memory cleanup in finally block

===============================================================================}

{-------------------------------------------------------------------------------
  Parse

  High-level flow for parsing the provided Args array:

  Step 0: Early help check
    - If '-h' or '--help' is anywhere in Args, we immediately print help and exit
      without attempting to parse anything else.

  Step 1: Reset/clear state for a fresh run
    - ResetParseState clears any previous error flag/message.
    - ClearPreviousResults frees previously parsed results so we start clean.

  Step 2: Iterate arguments one token at a time
    - For each token, we use the pre-tokenized data from ArgTokenizer. This provides:
        OptName  -> standardized switch form ('-x' or '--long')
        HasValue -> whether a value accompanied the switch
        ValueStr -> inline value text (e.g., '--name=value', '-n=value', '-nvalue')
      ArgTokenizer also handles conveniences like the PowerShell '.'-prefix
      quirk for atString combined short options.

  Step 3: Validate the option exists
    - We map OptName to an index in FOptions. If not found, we set an error and exit.

  Step 4: Seed the value with defaults and parse the typed value
    - Start from the option's DefaultValue (which also establishes the ArgType).
    - ParseOptionValue() then:
        * For booleans: the presence of the flag implies True, unless an explicit
          boolean value is provided (e.g., '--flag=false').
        * For non-booleans: if the value was not inline, it may consume the next
          token if it is not another option. Type conversion and required/empty
          checks are enforced here.

  Step 5: Emit callbacks and record the result
    - RunCallbacks() invokes any registered procedure/call-method callbacks.
    - AppendResult() stores the (LongOptName, ParsedValue) pair in FResults.

  Step 6: After all tokens, verify all required options were provided
    - CheckRequiredOptionsPresent() scans FOptions vs. FResults and reports the
      first missing required option, if any.

  Notes on error handling and resource safety:
    - On any error, SetError() is called and we exit Parse() early.
    - The local TArgValue variable is finalized in the finally block to avoid
      leaks even when exiting early.
-------------------------------------------------------------------------------}
procedure TArgParser.Parse(const Args: TStringDynArray);
var
  i: Integer;
  OptionIdx: Integer;
  OptName: string;
  Value: TArgValue;
  HasValue: Boolean;
  ValueStr: string;
  PosList: array of Integer;
  pIdx: Integer;
  j: Integer;
  Tokens: TArgTokenArray;
  tIdx: Integer;
  Tok: TArgToken;
  OldSplit: Boolean;
begin
  // Ensure local Value is in a safe state so Finalize(Value) is always valid.
  FillChar(Value, 0, SizeOf(Value));

  // Step 0: Early help handling (bypass normal parsing)
  // Note: Do not auto-show help here. Let normal parsing proceed so callers
  // can inspect the parsed results (including the built-in 'help' flag) and
  // decide whether to display usage/help and exit. This matches the README
  // examples and avoids surprising behavior where help is printed but the
  // caller's program continues executing.

  // Step 1: Prepare a clean parse state
  ResetParseState;
  ClearPreviousResults;

  Initialize(Value);
  try
  // Build ordered list of positional option indices (by PositionIndex)
    SetLength(PosList, 0);
    for j := 0 to High(FOptions) do
      if FOptions[j].IsPositional then
      begin
        SetLength(PosList, Length(PosList) + 1);
        PosList[High(PosList)] := j;
      end;
    // sort PosList by PositionIndex ascending (simple insertion sort)
    for j := 1 to High(PosList) do
    begin
      pIdx := PosList[j];
      i := j - 1;
      while (i >= 0) and (FOptions[PosList[i]].PositionIndex > FOptions[pIdx].PositionIndex) do
      begin
        PosList[i+1] := PosList[i];
        Dec(i);
      end;
      PosList[i+1] := pIdx;
    end;
    pIdx := 0; // pointer into PosList
    // Tokenize input and iterate tokens
    // Respect per-parser SplitCombinedShorts setting by temporarily setting
    // the module-level flag used by ArgTokenizer and restoring it afterwards.

    OldSplit := SplitCombinedShorts;
    try
      SplitCombinedShorts := FSplitCombinedShorts;
      Tokens := TokenizeArgs(Args);
    finally
      SplitCombinedShorts := OldSplit;
    end;
    tIdx := Low(Tokens);
    while tIdx <= High(Tokens) do
    begin
      Tok := Tokens[tIdx];
      OptName := Tok.OptName;
      HasValue := Tok.HasValue;
      ValueStr := Tok.ValueStr;

      // If OptName is empty, this is a positional or leftover token
      if (OptName = '') and (Tok.Kind = tkPositional) then
      begin
        // If we have a positional to fill, assign it
        if (Length(PosList) > 0) and (pIdx <= High(PosList)) then
        begin
          OptionIdx := PosList[pIdx];
          // prepare value string from current token; support greedy NArgs
          HasValue := True;
          ValueStr := Tok.ValueStr;
          if (FOptions[OptionIdx].NArgs = -1) and (FOptions[OptionIdx].ArgType in [atArray, atString]) then
          begin
            // collect subsequent non-option tokens as part of this positional
            // Note: multiple tokens are joined with commas for array/string greedy positionals
            while (tIdx < High(Tokens)) and (Tokens[tIdx+1].Kind = tkPositional) do
            begin
              ValueStr := ValueStr + ',' + Tokens[tIdx+1].ValueStr;
              Inc(tIdx);
            end;
          end;

          // parse and append result for positional
          Finalize(Value);
          Value := FOptions[OptionIdx].DefaultValue;
          if not ParseOptionValue(OptionIdx, HasValue, ValueStr, Tokens, tIdx, Value, '--' + FOptions[OptionIdx].LongOpt) then
            Exit;
          RunCallbacks(OptionIdx, Value);
          AppendResult(OptionIdx, Value);
          Inc(pIdx);
          Inc(tIdx);
          Continue;
        end
        else
        begin
          // No positional defined/left; treat as leftover if allowed
          if FParseKnown then
          begin
            AppendLeftover(Tok.Raw);
            Inc(tIdx);
            Continue;
          end
          else
          begin
            SetError('Unexpected positional token or stray argument: ' + Tok.Raw);
            Exit;
          end;
        end;
      end;

      // Step 3: Match the option definition
      OptionIdx := FindOption(OptName);
      if OptionIdx = -1 then
      begin
        if FParseKnown then
        begin
          AppendLeftover(Tok.Raw);
          Inc(tIdx);
          Continue;
        end
        else
        begin
          SetError('Unknown option: ' + Tok.Raw);
          Exit;
        end;
      end;

      // Step 4: Start from defaults and parse the typed value
      // Start from defaults and parse
      Finalize(Value);
      Value := FOptions[OptionIdx].DefaultValue;

      // If this option wants greedy collection (NArgs = -1) and is array or positional
      if (FOptions[OptionIdx].NArgs = -1) and (FOptions[OptionIdx].ArgType in [atArray, atString]) then
      begin
        // collect subsequent positional tokens into a comma-separated list
        if (tIdx < High(Tokens)) then
        begin
          if not HasValue then
          begin
            while (tIdx < High(Tokens)) and (Tokens[tIdx+1].Kind = tkPositional) do
            begin
              if ValueStr = '' then
                ValueStr := Tokens[tIdx+1].ValueStr
              else
                ValueStr := ValueStr + ',' + Tokens[tIdx+1].ValueStr;
              Inc(tIdx);
            end;
            HasValue := ValueStr <> '';
          end;
        end;
      end;

      if not ParseOptionValue(OptionIdx, HasValue, ValueStr, Tokens, tIdx, Value, Tok.Raw) then
        Exit;

      // Step 5: Run callbacks
      RunCallbacks(OptionIdx, Value);

      // If option allows multiple occurrences, always append a result; otherwise append as before
      if FOptions[OptionIdx].AllowMultiple then
        AppendResult(OptionIdx, Value)
      else
      begin
        // Remove previous result for this option (if present) and append fresh
        // We'll just append; GetX methods return the latest occurrence by scanning backwards
        AppendResult(OptionIdx, Value);
      end;

      Inc(tIdx);
    end;

    // Step 6: Ensure all required options were provided
    if not CheckRequiredOptionsPresent then
      Exit; // Error message already set
  finally
    // Always finalize the local Value to release managed fields
    Finalize(Value);
  end;
end;

{ Parse command-line arguments directly from ParamStr }
procedure TArgParser.ParseCommandLine;
var
  Args, LeftArgs, RightArgs: TStringDynArray;
  i: Integer;
begin
  Args := ParamStrToArray;
  SplitArgsAroundSeparator(Args, LeftArgs, RightArgs);
  
  // Parse left side as normal
  Parse(LeftArgs);
  
  // Populate FLeftovers with any right-side tokens
  if Length(RightArgs) > 0 then
  begin
    FLeftovers := nil;
    SetLength(FLeftovers, Length(RightArgs));
    for i := 0 to High(RightArgs) do
      FLeftovers[i] := RightArgs[i];
  end
  else
    FLeftovers := nil;
end;

procedure TArgParser.ParseCommandLineKnown(out Leftovers: TStringDynArray);
var
  Args, LeftArgs, RightArgs: TStringDynArray;
  i: Integer;
begin
  Args := ParamStrToArray;
  SplitArgsAroundSeparator(Args, LeftArgs, RightArgs);
  
  ParseKnownArgs(LeftArgs, Leftovers);

  if Length(RightArgs) > 0 then
  begin
    SetLength(Leftovers, Length(Leftovers) + Length(RightArgs));
    for i := 0 to High(RightArgs) do
      Leftovers[High(Leftovers) - High(RightArgs) + i] := RightArgs[i];
  end;
end;

procedure TArgParser.ShowHelp;
var
  i: Integer;
  MaxShort, MaxLong: Integer;
  Seen: TStringList;
  OptHelp, PosHelp: string;
begin
  { Calculate max widths for formatting }
  MaxShort := 1;  // ShortOpt is always a single character
  MaxLong := 0;
  // Compute max width for long option names and positional names separately
  for i := Low(FOptions) to High(FOptions) do
  begin
    if not FOptions[i].IsPositional then
    begin
      if Length(FOptions[i].LongOpt) > MaxLong then
        MaxLong := Length(FOptions[i].LongOpt);
    end
    else
    begin
      // positionals will be handled later; keep MaxLong for options
    end;
  end;
  
  WriteLn('Usage: ' + FUsage);
  WriteLn;
  WriteLn('Options:');
  Seen := TStringList.Create;
  try
    Seen.CaseSensitive := False;
    for i := Low(FOptions) to High(FOptions) do
    begin
      if FOptions[i].IsPositional then
        Continue; // skip positionals in the Options section

      // If we've already printed this long option (or short if long is empty), skip
      if (FOptions[i].LongOpt <> '') then
      begin
        if Seen.IndexOf('--' + FOptions[i].LongOpt) >= 0 then
          Continue;
        Seen.Add('--' + FOptions[i].LongOpt);
      end
      else if FOptions[i].ShortOpt <> #0 then
      begin
        if Seen.IndexOf('-' + FOptions[i].ShortOpt) >= 0 then
          Continue;
        Seen.Add('-' + FOptions[i].ShortOpt);
      end;

      Write('  ');
      // Only print short option if defined (non-zero char).
      if FOptions[i].ShortOpt <> #0 then
        Write('-' + FOptions[i].ShortOpt + ', ')
      else
        Write('    '); // spaces to align when no short option

      Write('--' + FOptions[i].LongOpt);
      Write(Space(MaxLong - Length(FOptions[i].LongOpt)));
      Write('  ');
      // Append "(required)" to help text for required options
      OptHelp := FOptions[i].HelpText;
      if FOptions[i].Required then
        OptHelp := OptHelp + ' (required)';
      WriteLn(OptHelp);
    end;
  finally
    Seen.Free;
  end;

  { Print positional arguments separately to avoid showing them as `--name` }
  // Find any positionals
  for i := Low(FOptions) to High(FOptions) do
    if FOptions[i].IsPositional then
    begin
      WriteLn;
      WriteLn('Positionals:');
      Break;
    end;

  // Compute max positional name length for alignment
  MaxLong := 0;
  for i := Low(FOptions) to High(FOptions) do
    if FOptions[i].IsPositional then
      if Length(FOptions[i].LongOpt) > MaxLong then
        MaxLong := Length(FOptions[i].LongOpt);

  // Dedupe positionals too (in case registration was duplicated)
  Seen := TStringList.Create;
  try
    Seen.CaseSensitive := False;
    for i := Low(FOptions) to High(FOptions) do
    begin
      if not FOptions[i].IsPositional then
        Continue;

      if Seen.IndexOf(FOptions[i].LongOpt) >= 0 then
        Continue;
      Seen.Add(FOptions[i].LongOpt);

      Write('  ');
      // Print positional name without dashes
      Write(FOptions[i].LongOpt);
      Write(Space(MaxLong - Length(FOptions[i].LongOpt)));
      Write('  ');
      // Append "(required)" for required positionals
      PosHelp := FOptions[i].HelpText;
      if FOptions[i].Required then
        PosHelp := PosHelp + ' (required)';
      WriteLn(PosHelp);
    end;
  finally
    Seen.Free;
  end;

  Done;
end;

procedure TArgParser.ShowUsage;
begin
  WriteLn('Usage: ' + FUsage);
  // Free resources after showing usage to avoid leaks on error paths
  Done;
  // Also clear stored strings so heaptrc doesn't report them as leaks
  FError := '';
  FUsage := '';
end;

procedure TArgParser.SetUsage(const AUsage: string);
begin
  FUsage := AUsage;
end;

procedure TArgParser.SetError(const AError: string);
begin
  FHasError := True;
  FError := AError;
  // Do not clean up here so caller can still access Error and then ShowUsage.
end;

function TArgParser.HasError: Boolean;
begin
  Result := FHasError;
end;

function TArgParser.OptionCount: Integer;
begin
  Result := Length(FOptions);
end;

function TArgParser.GetLeftovers: TStringDynArray;
var
  i: Integer;
begin
  if Length(FLeftovers) = 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, Length(FLeftovers));
  for i := 0 to High(FLeftovers) do
    Result[i] := FLeftovers[i];
end;

procedure TArgParser.AddOption(const Option: TArgOption);
var
  LOption: TArgOption;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  // copy const param to local so we can set defaults
  LOption := Option;
  if not LOption.IsPositional then
    LOption.PositionIndex := -1;
  // ensure NArgs is defined (no-op, kept for clarity)
  if LOption.NArgs = 0 then
    LOption.NArgs := 0;
  FOptions[High(FOptions)] := LOption;
  // add to lookup map for fast finding
  if Assigned(FLookup) then
  begin
    // Only register non-positional options in the lookup. Positionals are
    // matched by position, not by `--name` form.
    if not LOption.IsPositional then
    begin
      // long form
      if LOption.LongOpt <> '' then
        FLookup.AddObject('--' + LOption.LongOpt, TObject(High(FOptions)));
      // short form
      if LOption.ShortOpt <> #0 then
        FLookup.AddObject('-' + LOption.ShortOpt, TObject(High(FOptions)));
    end;
  end;
end;

procedure TArgParser.AddString(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: string = ''; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue := CreateArgValue(atString);
  DefaultValue.Str := Default;
  Add(ShortOpt, LongOpt, atString, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddInteger(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Integer = 0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue := CreateArgValue(atInteger);
  DefaultValue.Int := Default;
  Add(ShortOpt, LongOpt, atInteger, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddFloat(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Double = 0.0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue := CreateArgValue(atFloat);
  DefaultValue.Flt := Default;
  Add(ShortOpt, LongOpt, atFloat, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Boolean = False; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue := CreateArgValue(atBoolean);
  DefaultValue.Bool := Default;
  Add(ShortOpt, LongOpt, atBoolean, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue := CreateArgValue(atArray);
  Add(ShortOpt, LongOpt, atArray, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddPositional(const Name: string; const ArgType: TArgType; const HelpText: string; const Default: string = ''; const Required: Boolean = False; const NArgs: Integer = 0);
var
  Option: TArgOption;
  DefaultValue: TArgValue;
begin
  // Prepare default value container
  DefaultValue := CreateArgValue(ArgType);
  DefaultValue.Str := Default;

  Option.ShortOpt := #0;
  Option.LongOpt := Name;
  Option.ArgType := ArgType;
  Option.HelpText := HelpText;
  Option.Callback := nil;
  Option.CallbackClass := nil;
  Option.Required := Required;
  Option.DefaultValue := DefaultValue;
  Option.IsPositional := True;
  Option.PositionIndex := Length(FOptions);
  Option.AllowMultiple := False;
  Option.NArgs := NArgs;

  AddOption(Option);
end;

procedure TArgParser.AppendLeftover(const Token: string);
begin
  AppendToArray(FLeftovers, Token);
end;

procedure TArgParser.SplitArgsAroundSeparator(const Args: TStringDynArray; out LeftArgs, RightArgs: TStringDynArray);
var
  i, dashIdx: Integer;
begin
  // Find `--` separator
  dashIdx := -1;
  for i := Low(Args) to High(Args) do
    if Args[i] = '--' then
    begin
      dashIdx := i;
      Break;
    end;

  if dashIdx >= 0 then
  begin
    // Split around `--`
    if dashIdx > Low(Args) then
    begin
      SetLength(LeftArgs, dashIdx - Low(Args));
      for i := 0 to Length(LeftArgs)-1 do
        LeftArgs[i] := Args[Low(Args) + i];
    end
    else
      LeftArgs := nil;

    if dashIdx < High(Args) then
    begin
      SetLength(RightArgs, High(Args) - dashIdx);
      for i := 0 to Length(RightArgs)-1 do
        RightArgs[i] := Args[dashIdx + 1 + i];
    end
    else
      RightArgs := nil;
  end
  else
  begin
    // No separator found
    LeftArgs := Args;
    RightArgs := nil;
  end;
end;

procedure TArgParser.ParseKnownArgs(const Args: TStringDynArray; out Leftovers: TStringDynArray);
begin
  FLeftovers := nil;
  FParseKnown := True;
  Parse(Args);
  Leftovers := FLeftovers;
  FParseKnown := False;
end;

procedure TArgParser.SetAllowMultiple(const LongOpt: string; const Value: Boolean);
var
  idx: Integer;
begin
  idx := FindOption(LongOpt);
  if idx >= 0 then
    FOptions[idx].AllowMultiple := Value;
end;

function TArgParser.GetAllString(const LongOpt: string): TStringDynArray;
var
  i: Integer;
  tmp: TStringDynArray;
begin
  SetLength(tmp, 0);
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atString) then
    begin
      SetLength(tmp, Length(tmp) + 1);
      tmp[High(tmp)] := FResults[i].Value.Str;
    end;
  Result := tmp;
end;

function TArgParser.GetAllInteger(const LongOpt: string): TIntegerDynArray;
var
  i: Integer;
  tmp: TIntegerDynArray;
begin
  SetLength(tmp, 0);
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atInteger) then
    begin
      SetLength(tmp, Length(tmp) + 1);
      tmp[High(tmp)] := FResults[i].Value.Int;
    end;
  Result := tmp;
end;

function TArgParser.GetAllFloat(const LongOpt: string): TDoubleDynArray;
var
  i: Integer;
  tmp: TDoubleDynArray;
begin
  SetLength(tmp, 0);
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atFloat) then
    begin
      SetLength(tmp, Length(tmp) + 1);
      tmp[High(tmp)] := FResults[i].Value.Flt;
    end;
  Result := tmp;
end;

function TArgParser.GetAllBoolean(const LongOpt: string): TBooleanDynArray;
var
  i: Integer;
  tmp: TBooleanDynArray;
begin
  SetLength(tmp, 0);
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atBoolean) then
    begin
      SetLength(tmp, Length(tmp) + 1);
      tmp[High(tmp)] := FResults[i].Value.Bool;
    end;
  Result := tmp;
end;

function TArgParser.GetAllArray(const LongOpt: string): TStringDynArrayArray;
var
  i: Integer;
  tmp: TStringDynArrayArray;
begin
  SetLength(tmp, 0);
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atArray) then
    begin
      SetLength(tmp, Length(tmp) + 1);
      tmp[High(tmp)] := FResults[i].Value.Arr;
    end;
  Result := tmp;
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultArr: array of string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
  i: Integer;
begin
  DefaultValue := CreateArgValue(atArray);
  SetLength(DefaultValue.Arr, Length(DefaultArr));
  for i := 0 to High(DefaultArr) do
    DefaultValue.Arr[i] := DefaultArr[i];
  Add(ShortOpt, LongOpt, atArray, HelpText, nil, nil, Required, DefaultValue);
end;

function TArgParser.GetString(const LongOpt: string): string;
var
  i: Integer;
begin
  for i := High(FResults) downto Low(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atString) then
    begin
      Result := FResults[i].Value.Str;
      Exit;
    end;
  Result := '';
end;

function TArgParser.GetInteger(const LongOpt: string): Integer;
var
  i: Integer;
begin
  for i := High(FResults) downto Low(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atInteger) then
    begin
      Result := FResults[i].Value.Int;
      Exit;
    end;
  Result := 0;
end;

function TArgParser.GetFloat(const LongOpt: string): Double;
var
  i: Integer;
begin
  for i := High(FResults) downto Low(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atFloat) then
    begin
      Result := FResults[i].Value.Flt;
      Exit;
    end;
  Result := 0.0;
end;

function TArgParser.GetBoolean(const LongOpt: string): Boolean;
var
  i: Integer;
begin
  for i := High(FResults) downto Low(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atBoolean) then
    begin
      Result := FResults[i].Value.Bool;
      Exit;
    end;
  Result := False;
end;

function TArgParser.GetArray(const LongOpt: string): TStringDynArray;
var
  i: Integer;
begin
  for i := High(FResults) downto Low(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atArray) then
    begin
      Result := FResults[i].Value.Arr;
      Exit;
    end;
  Result := nil;
end;

procedure TArgParser.Done;
var
  i: Integer;
begin
  // Free results
  for i := 0 to High(FResults) do
    Finalize(FResults[i].Value);
  FResults := nil;

  // Free options (strings + DefaultValue’s managed fields)
  for i := 0 to High(FOptions) do
    Finalize(FOptions[i]);
  FOptions := nil;
  // Free lookup
  if Assigned(FLookup) then
  begin
    FLookup.Free;
    FLookup := nil;
  end;
  // Do NOT clear FError/FUsage/FHasError here.
  // Leaving them intact allows callers to read Error and ShowUsage after failure.
end;

class operator TArgParser.Finalize(var r: TArgParser);
begin
  r.Done;
end;

end.
