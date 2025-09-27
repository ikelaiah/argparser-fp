
//-------------------------------------------------------------------------------
// Unit: ArgParser
//
// A lightweight, record-based command-line argument parser for Free Pascal.
// Supports string, integer, float, boolean, and array types.
// Provides methods to define options, parse command-line arguments, and retrieve parsed values.
//-------------------------------------------------------------------------------
unit ArgParser;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Types, StrUtils;

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
    NArgs: Integer;        { 0 = single token (default), -1 = greedy (consume until next option), >0 = fixed count }
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

    { Locate option by input switch. }
    function FindOption(const Opt: string): Integer;
    { Convert string to TArgValue based on ArgType. }
    function ParseValue(const ValueStr: string; const ArgType: TArgType; var Value: TArgValue): Boolean;
    { Internal helper to add an option record. }
    procedure AddOption(const Option: TArgOption);
    { Record a parsing error. }
    procedure SetError(const AError: string);
    { Retrieve the current error message. }
    function GetError: string;
    { Helpers to make Parse more readable }
    { Returns True if any arg equals '-h' or '--help' }
    function IsHelpRequested(const Args: TStringDynArray): Boolean;
    { Clears error flags/messages before a new Parse run }
    procedure ResetParseState;
    { Finalizes and clears FResults from any previous Parse run }
    procedure ClearPreviousResults;
    { Normalizes current token into (OptName, HasValue, ValueStr); supports --name=value, -n=value, -nvalue and the PowerShell split quirk }
    function NormalizeToken(const Args: TStringDynArray; var i: Integer; out OptName: string; out HasValue: Boolean; out ValueStr: string): Boolean;
    { Populates typed Value for the option; may consume next token; enforces required/empty rules and type parsing }
    function ParseOptionValue(const OptionIdx: Integer; var HasValue: Boolean; var ValueStr: string; const Args: TStringDynArray; var i: Integer; var Value: TArgValue; const CurrentOpt: string): Boolean;
    { Invokes any registered callbacks for this option }
    procedure RunCallbacks(const OptionIdx: Integer; const Value: TArgValue);
    { Appends the parsed (Name, Value) pair to FResults }
    procedure AppendResult(const OptionIdx: Integer; const Value: TArgValue);
  procedure AppendLeftover(const Token: string);
    { Ensures all options marked Required=True were provided at least once }
    function CheckRequiredOptionsPresent: Boolean;
    { Parse the provided command-line arguments array. }
    procedure Parse(const Args: TStringDynArray);
    procedure ParseKnown(const Args: TStringDynArray; out Leftovers: TStringDynArray);
  public
    { Initialize parser state. Call before adding any options. }
    procedure Init;
    { Add a new option with all parameters including callbacks. }
    procedure Add(const ShortOpt: Char; const LongOpt: string; const ArgType: TArgType; const HelpText: string; const Callback: TArgCallback; const CallbackClass: TArgCallbackClass; const Required: Boolean; const DefaultValue: TArgValue);
    { Parse command-line arguments directly from ParamStr }
    procedure ParseCommandLine;
  { Parse command-line with support for `--` separator; leftovers can be retrieved via GetLeftovers }
  procedure ParseCommandLineKnown(out Leftovers: TStringDynArray);
    { Returns True if an error occurred during parsing. }
    function HasError: Boolean;
    { Read-only property to get error message. }
    property Error: string read GetError;
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
      - NArgs: number of tokens to consume: 0 = single token (default), >0 fixed count, -1 = greedy (consume until next option/end)
    Positionals are matched in the order they are added. Greedy positionals (NArgs=-1)
    will consume all remaining non-option tokens.
  }
  procedure AddPositional(const Name: string; const ArgType: TArgType; const HelpText: string; const Default: string = ''; const Required: Boolean = False; const NArgs: Integer = 0);
  { Return all occurrences for options (accumulated).
    Use these helpers when an option may appear multiple times (AllowMultiple) or when
    working with array-like options. Values are returned in the order parsed.
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
    { Accessors for parsed values by long option name. }
    function GetString(const LongOpt: string): string;
    function GetInteger(const LongOpt: string): Integer;
    function GetFloat(const LongOpt: string): Double;
    function GetBoolean(const LongOpt: string): Boolean;
    function GetArray(const LongOpt: string): TStringDynArray;
    procedure Done;
    class operator Finalize(var r: TArgParser);
  end;

implementation

function ParamStrToArray: TStringDynArray;
var
  i: Integer;
begin
  Result := nil; // Explicitly initialize to avoid warning
  SetLength(Result, ParamCount);
  for i := 1 to ParamCount do
    Result[i-1] := ParamStr(i);
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
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FOptions) do
  begin
    if (Length(Opt) = 2) and (Opt[1] = '-') and (UpCase(FOptions[i].ShortOpt) = UpCase(Opt[2])) then
    begin
      Result := i;
      Exit;
    end
    else if (Length(Opt) > 2) and (Opt[1] = '-') and (Opt[2] = '-') and
      SameText(FOptions[i].LongOpt, Copy(Opt, 3, MaxInt)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TArgParser.ParseValue(const ValueStr: string; const ArgType: TArgType; var Value: TArgValue): Boolean;
var
  i: Integer;
  s: string;
  Parts: TStringDynArray;
begin
  Result := False;

  // Finalize previous value to prevent memory leaks
  Finalize(Value);

  Value.ArgType := ArgType;
  
  // Initialize all fields to avoid potential memory issues
  Value.Str := '';
  Value.Int := 0;
  Value.Flt := 0.0;
  Value.Bool := False;
  Value.Arr := nil;
  
  case ArgType of
    atString:
      begin
        Value.Str := ValueStr;
        Result := True;
      end;
    atInteger:
      begin
        if TryStrToInt(ValueStr, Value.Int) then
          Result := True;
      end;
    atFloat:
      begin
        if TryStrToFloat(ValueStr, Value.Flt) then
          Result := True;
      end;
    atBoolean:
      begin
        if TryStrToBool(ValueStr, Value.Bool) then
          Result := True;
      end;
    atArray:
      begin
        // Use SplitString from SysUtils
        Parts := SplitString(ValueStr, ',');
        for i := 0 to High(Parts) do
        begin
          s := Trim(Parts[i]);
          if s <> '' then
          begin
            SetLength(Value.Arr, Length(Value.Arr)+1);
            Value.Arr[High(Value.Arr)] := s;
          end;
        end;
        Result := True;
      end;
  end;
end;

function TArgParser.IsHelpRequested(const Args: TStringDynArray): Boolean;
var
  k: Integer;
begin
  { Quick scan for help flags. If found, Parse() will ShowHelp and exit early. }
  Result := False;
  for k := Low(Args) to High(Args) do
    if (Args[k] = '-h') or (Args[k] = '--help') then
    begin
      Result := True;
      Exit;
    end;
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

function TArgParser.NormalizeToken(const Args: TStringDynArray; var i: Integer; out OptName: string; out HasValue: Boolean; out ValueStr: string): Boolean;
var
  CurrentOpt: string;
  EqualPos: Integer;
  ShortOptStr: string;
  OptionIdx: Integer;
begin
  { Normalize the current argument token into a consistent shape:
    - OptName: '-x' or '--long'
    - HasValue/ValueStr: inline value if present (e.g., --name=value, -n=value, -nvalue)

    It also validates that the token starts with '-'.
    Special handling: combined short option with concatenated value ('-finput.txt')
    and the PowerShell split quirk where the next token starting with '.' is appended
    for string options.
  }
  Result := False;
  CurrentOpt := Args[i];
  OptName := CurrentOpt;
  ValueStr := '';
  HasValue := False;

  // Handle --name=value and -o=value
  EqualPos := Pos('=', CurrentOpt);
  if (EqualPos > 0) and (Length(CurrentOpt) > 2) and (CurrentOpt[1] = '-') then
  begin
    ValueStr := Copy(CurrentOpt, EqualPos + 1, Length(CurrentOpt));
    OptName := Copy(CurrentOpt, 1, EqualPos - 1);
    HasValue := True;
    Result := True;
    Exit;
  end
  // Handle combined short option with value (e.g. -fvalue)
  else if (Length(CurrentOpt) > 2)
       and (CurrentOpt[1] = '-')
       and (CurrentOpt[2] <> '-') then
  begin
    ShortOptStr := Copy(CurrentOpt, 1, 2); // '-f'
    OptionIdx := FindOption(ShortOptStr);
    if OptionIdx = -1 then
    begin
      SetError('Unknown option: ' + ShortOptStr);
      Exit;
    end;

    ValueStr := Copy(CurrentOpt, 3, MaxInt);
    HasValue := True;
    OptName := ShortOptStr;
    // PowerShell quirk: append next token starting with '.' for string options
    if (i < High(Args))
       and (FOptions[OptionIdx].ArgType = atString)
       and (Length(Args[i+1]) > 0)
       and (Args[i+1][1] = '.') then
    begin
      ValueStr := ValueStr + Args[i+1];
      Inc(i);
    end;
    Result := True;
    Exit;
  end;

  // Validate starts with '-'
  if (Length(CurrentOpt) = 0) or (CurrentOpt[1] <> '-') then
  begin
    // Not an option token; leave OptName empty to signal positional or leftover
    OptName := '';
    Result := True;
    Exit;
  end;

  // No normalization needed; return as-is
  OptName := CurrentOpt;
  Result := True;
end;

function TArgParser.ParseOptionValue(const OptionIdx: Integer; var HasValue: Boolean; var ValueStr: string; const Args: TStringDynArray; var i: Integer; var Value: TArgValue; const CurrentOpt: string): Boolean;
begin
  { Based on option type and HasValue/ValueStr, populate Value.
    - Booleans: presence implies True unless an explicit value is provided.
    - Non-booleans: use inline value if present; otherwise consume next token if it isn't an option.
    - Required string options: reject empty values.
    - Perform type conversion using ParseValue and surface clear errors.
  }
  Result := False;

  // Boolean options: present means true unless explicit value provided
  if FOptions[OptionIdx].ArgType = atBoolean then
  begin
    // Support --no-<long> negation form
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
  else if (i < High(Args)) and ((Length(Args[i+1]) = 0) or (Args[i+1][1] <> '-')) then
  begin
    ValueStr := Args[i+1];
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
    Inc(i);
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
        SetError('Missing required option: --' + FOptions[j].LongOpt);
        Result := False;
        Exit;
      end;
    end;
  end;
end;

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
    - For each token, we first normalize it using NormalizeToken(). This produces:
        OptName  -> standardized switch form ('-x' or '--long')
        HasValue -> whether a value accompanied the switch
        ValueStr -> inline value text (e.g., '--name=value', '-n=value', '-nvalue')
      NormalizeToken also performs small conveniences like the PowerShell '.'-prefix
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
    // Step 2: Walk through the arguments left-to-right
    i := Low(Args);
    while i <= High(Args) do
    begin
      // 2.1 Normalize the current token -> OptName, HasValue, ValueStr
      if not NormalizeToken(Args, i, OptName, HasValue, ValueStr) then
        Exit; // SetError was already called inside NormalizeToken

      // If NormalizeToken left OptName empty, this is a positional or leftover token
      if OptName = '' then
      begin
        // If we have a positional to fill, assign it
        if (Length(PosList) > 0) and (pIdx <= High(PosList)) then
        begin
          OptionIdx := PosList[pIdx];
          // prepare value string from current token; support greedy NArgs
          HasValue := True;
          ValueStr := Args[i];
          if (FOptions[OptionIdx].NArgs = -1) and (FOptions[OptionIdx].ArgType in [atArray, atString]) then
          begin
            // collect subsequent non-option tokens as part of this positional
            while (i < High(Args)) and (Length(Args[i+1])>0) and (Args[i+1][1] <> '-') do
            begin
              ValueStr := ValueStr + ',' + Args[i+1];
              Inc(i);
            end;
          end;

          // parse and append result for positional
          Finalize(Value);
          Value := FOptions[OptionIdx].DefaultValue;
          if not ParseOptionValue(OptionIdx, HasValue, ValueStr, Args, i, Value, '--' + FOptions[OptionIdx].LongOpt) then
            Exit;
          RunCallbacks(OptionIdx, Value);
          AppendResult(OptionIdx, Value);
          Inc(pIdx);
          Inc(i);
          Continue;
        end
        else
        begin
          // No positional defined/left; treat as leftover if allowed
          if FParseKnown then
          begin
            AppendLeftover(Args[i]);
            Inc(i);
            Continue;
          end
          else
          begin
            SetError('Unexpected positional token or stray argument: ' + Args[i]);
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
          AppendLeftover(OptName);
          Inc(i);
          Continue;
        end
        else
        begin
          SetError('Unknown option: ' + OptName);
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
        // collect subsequent non-option tokens into a comma-separated list for arrays
        if (i < High(Args)) then
        begin
          // If inline value was provided, start with it
          if HasValue and (ValueStr <> '') then
          begin
            // start with ValueStr
          end
          else
          begin
            // consume following tokens until one starts with '-'
            while (i < High(Args)) and (Length(Args[i+1])>0) and (Args[i+1][1] <> '-') do
            begin
              if ValueStr = '' then
                ValueStr := Args[i+1]
              else
                ValueStr := ValueStr + ',' + Args[i+1];
              Inc(i);
            end;
            HasValue := ValueStr <> '';
          end;
        end;
      end;

      if not ParseOptionValue(OptionIdx, HasValue, ValueStr, Args, i, Value, OptName) then
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

      Inc(i);
    end;

    // Step 6: Ensure all required options were provided
    if not CheckRequiredOptionsPresent then
      Exit; // Error message already set
  finally
    // Always finalize the local Value to release managed fields
    Finalize(Value);
  end;
end;

procedure TArgParser.ParseKnown(const Args: TStringDynArray; out Leftovers: TStringDynArray);
begin
  // Delegate to the public-friendly ParseKnownArgs implementation
  ParseKnownArgs(Args, Leftovers);
end;

{ Parse command-line arguments directly from ParamStr }
procedure TArgParser.ParseCommandLine;
var
  Args: TStringDynArray;
  i, dashIdx: Integer;
  LeftArgs, RightArgs: TStringDynArray;
begin
  Args := ParamStrToArray;
  // Detect `--` separator and split like ParseCommandLineKnown did
  dashIdx := -1;
  for i := Low(Args) to High(Args) do
    if Args[i] = '--' then
    begin
      dashIdx := i;
      Break;
    end;

  if dashIdx >= 0 then
  begin
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
    LeftArgs := Args;
    RightArgs := nil;
  end;

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
  Args: TStringDynArray;
  i, dashIdx: Integer;
  LeftArgs, RightArgs: TStringDynArray;
begin
  Args := ParamStrToArray;
  dashIdx := -1;
  for i := Low(Args) to High(Args) do
    if Args[i] = '--' then
    begin
      dashIdx := i;
      Break;
    end;

  if dashIdx >= 0 then
  begin
    if dashIdx > Low(Args) then
    begin
      SetLength(LeftArgs, dashIdx - Low(Args));
      // copy elements safely
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
    LeftArgs := Args;
    RightArgs := nil;
  end;

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
begin
  { Calculate max widths for formatting }
  MaxShort := 1;  // ShortOpt is always a single character
  MaxLong := 0;
  for i := Low(FOptions) to High(FOptions) do
  begin
    if Length(FOptions[i].LongOpt) > MaxLong then
      MaxLong := Length(FOptions[i].LongOpt);
  end;
  
  WriteLn('Usage: ' + FUsage);
  WriteLn;
  WriteLn('Options:');

  for i := Low(FOptions) to High(FOptions) do
  begin
    Write('  ');
    // Only print short option if defined (non-zero char). Positionals may
    // have ShortOpt = #0; in that case, omit the short form and align output.
    if FOptions[i].ShortOpt <> #0 then
      Write('-' + FOptions[i].ShortOpt + ', ')
    else
      Write('    '); // spaces to align when no short option

    Write('--' + FOptions[i].LongOpt);
    Write(Space(MaxLong - Length(FOptions[i].LongOpt)));
    Write('  ');
    WriteLn(FOptions[i].HelpText);
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

function TArgParser.GetError: string;
begin
  Result := FError;
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
end;

procedure TArgParser.AddString(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: string = ''; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  // Initialize all fields
  DefaultValue.ArgType := atString;
  DefaultValue.Str := Default;
  DefaultValue.Int := 0;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := False;
  DefaultValue.Arr := nil;
  
  Add(ShortOpt, LongOpt, atString, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddInteger(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Integer = 0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  // Initialize all fields
  DefaultValue.ArgType := atInteger;
  DefaultValue.Str := '';
  DefaultValue.Int := Default;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := False;
  DefaultValue.Arr := nil;
  
  Add(ShortOpt, LongOpt, atInteger, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddFloat(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Double = 0.0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  // Initialize all fields
  DefaultValue.ArgType := atFloat;
  DefaultValue.Str := '';
  DefaultValue.Int := 0;
  DefaultValue.Flt := Default;
  DefaultValue.Bool := False;
  DefaultValue.Arr := nil;
  
  Add(ShortOpt, LongOpt, atFloat, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Boolean = False; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  // Initialize all fields
  DefaultValue.ArgType := atBoolean;
  DefaultValue.Str := '';
  DefaultValue.Int := 0;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := Default;
  DefaultValue.Arr := nil;
  
  Add(ShortOpt, LongOpt, atBoolean, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  // Initialize all fields
  DefaultValue.ArgType := atArray;
  DefaultValue.Str := '';
  DefaultValue.Int := 0;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := False;
  DefaultValue.Arr := nil;
  
  Add(ShortOpt, LongOpt, atArray, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddPositional(const Name: string; const ArgType: TArgType; const HelpText: string; const Default: string = ''; const Required: Boolean = False; const NArgs: Integer = 0);
var
  Option: TArgOption;
  DefaultValue: TArgValue;
  idx: Integer;
begin
  // Prepare default value container
  DefaultValue.ArgType := ArgType;
  DefaultValue.Str := Default;
  DefaultValue.Int := 0;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := False;
  DefaultValue.Arr := nil;

  Option.ShortOpt := #0;
  Option.LongOpt := Name;
  Option.ArgType := ArgType;
  Option.HelpText := HelpText;
  Option.Callback := nil;
  Option.CallbackClass := nil;
  Option.Required := Required;
  Option.DefaultValue := DefaultValue;
  Option.IsPositional := True;
  // PositionIndex: append at end
  idx := 0;
  for idx := 0 to High(FOptions) do ; // noop to find High
  Option.PositionIndex := Length(FOptions); // next index
  Option.AllowMultiple := False;
  Option.NArgs := NArgs;

  AddOption(Option);
end;

procedure TArgParser.AppendLeftover(const Token: string);
begin
  SetLength(FLeftovers, Length(FLeftovers) + 1);
  FLeftovers[High(FLeftovers)] := Token;
end;

procedure TArgParser.ParseKnownArgs(const Args: TStringDynArray; out Leftovers: TStringDynArray);
begin
  FLeftovers := nil;
  FParseKnown := True;
  Parse(Args);
  Leftovers := FLeftovers;
  FParseKnown := False;
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
  i, cnt: Integer;
  tmp: TBooleanDynArray;
begin
  cnt := 0;
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atBoolean) then
      Inc(cnt);
  SetLength(tmp, cnt);
  cnt := 0;
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atBoolean) then
    begin
      tmp[cnt] := FResults[i].Value.Bool;
      Inc(cnt);
    end;
  Result := tmp;
end;

function TArgParser.GetAllArray(const LongOpt: string): TStringDynArrayArray;
var
  i, cnt: Integer;
  tmp: TStringDynArrayArray;
begin
  cnt := 0;
begin
  cnt := 0;
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atArray) then
      Inc(cnt);
  SetLength(tmp, cnt);
  cnt := 0;
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atArray) then
    begin
      tmp[cnt] := FResults[i].Value.Arr;
      Inc(cnt);
    end;
  Result := tmp;
end;
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultArr: array of string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
  i: Integer;
begin
  DefaultValue.ArgType := atArray;
  // Initialize all fields to avoid potential memory issues
  DefaultValue.Str := '';
  DefaultValue.Int := 0;
  DefaultValue.Flt := 0.0;
  DefaultValue.Bool := False;
  
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
  // Do NOT clear FError/FUsage/FHasError here.
  // Leaving them intact allows callers to read Error and ShowUsage after failure.
end;

class operator TArgParser.Finalize(var r: TArgParser);
begin
  r.Done;
end;

end.
