
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

  { TArgParser: Main record to define options, parse arguments, and access results. }
  TArgParser = record
  private
    FOptions: TOptionsArray;  { Defined command-line options }
    FUsage: string;           { Custom usage banner text }
    FError: string;           { Error message if parsing fails }
    FHasError: Boolean;       { Flag indicating parsing error }
    FResults: TParseResults;  { Parsed results }

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
    { Ensures all options marked Required=True were provided at least once }
    function CheckRequiredOptionsPresent: Boolean;
    { Parse the provided command-line arguments array. }
    procedure Parse(const Args: TStringDynArray);
  public
    { Initialize parser state. Call before adding any options. }
    procedure Init;
    { Add a new option with all parameters including callbacks. }
    procedure Add(const ShortOpt: Char; const LongOpt: string; const ArgType: TArgType; const HelpText: string; const Callback: TArgCallback; const CallbackClass: TArgCallbackClass; const Required: Boolean; const DefaultValue: TArgValue);
    { Parse command-line arguments directly from ParamStr }
    procedure ParseCommandLine;
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
    SetError('Invalid argument format: ' + CurrentOpt);
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

procedure TArgParser.Parse(const Args: TStringDynArray);
var
  i: Integer;
  OptionIdx: Integer;
  OptName: string;
  Value: TArgValue;
  HasValue: Boolean;
  ValueStr: string;
begin
  // Make Finalize(Value) safe even if we exit early on an error.
  FillChar(Value, 0, SizeOf(Value));

  // Early help handling
  if IsHelpRequested(Args) then
  begin
    ShowHelp;
    Exit;
  end;

  // Initialize parser state and clear old results
  ResetParseState;
  ClearPreviousResults;

  Initialize(Value);
  try
    i := Low(Args);
    while i <= High(Args) do
    begin
      // 1) Normalize current token -> OptName, HasValue, ValueStr
      if not NormalizeToken(Args, i, OptName, HasValue, ValueStr) then
        Exit;

      // 2) Validate option exists
      OptionIdx := FindOption(OptName);
      if OptionIdx = -1 then
      begin
        SetError('Unknown option: ' + OptName);
        Exit;
      end;

      // 3) Initialize with the option's default value (establishes ArgType)
      Finalize(Value);
      Value := FOptions[OptionIdx].DefaultValue;

      // 4) Parse typed value and optionally consume next token when appropriate
      if not ParseOptionValue(OptionIdx, HasValue, ValueStr, Args, i, Value, OptName) then
        Exit;

      // 5) Execute callbacks and store result
      RunCallbacks(OptionIdx, Value);
      AppendResult(OptionIdx, Value);

      Inc(i);
    end;

    // 6) Final validation for missing required options
    if not CheckRequiredOptionsPresent then
      Exit;
  finally
    Finalize(Value);
  end;
end;

{ Parse command-line arguments directly from ParamStr }
procedure TArgParser.ParseCommandLine;
var
  Args: TStringDynArray;
begin
  Args := ParamStrToArray;
  Parse(Args);
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
    Write('-' + FOptions[i].ShortOpt);
    Write(', --' + FOptions[i].LongOpt);
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

procedure TArgParser.AddOption(const Option: TArgOption);
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)] := Option;
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
