
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
    if (Length(Opt) = 2) and (Opt[1] = '-') and (FOptions[i].ShortOpt = Opt[2]) then
    begin
      Result := i;
      Exit;
    end
    else if (Length(Opt) > 2) and (Opt[1] = '-') and (Opt[2] = '-') and
      (FOptions[i].LongOpt = Copy(Opt, 3, MaxInt)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TArgParser.ParseValue(const ValueStr: string; const ArgType: TArgType; var Value: TArgValue): Boolean;
var
  i: Integer;
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
        
        // Initialize array with the correct size
        SetLength(Value.Arr, Length(Parts));
        
        // Copy and trim each part
        for i := 0 to High(Parts) do
          Value.Arr[i] := Trim(Parts[i]);
        
        // Explicitly clear the Parts array to avoid memory leaks
        SetLength(Parts, 0);
        
        Result := True;
      end;
  end;
end;

procedure TArgParser.Parse(const Args: TStringDynArray); 
var
  i, j, OptionIdx: Integer;
  CurrentOpt: string;
  Value: TArgValue;
  FoundRequired: Boolean;
  HasValue: Boolean;
  ValueStr: string;
begin
  { Step 0: Check for help flag first - special case that exits early }
  for i := Low(Args) to High(Args) do
    if (Args[i] = '-h') or (Args[i] = '--help') then
    begin
      ShowHelp;
      Exit;
    end;
  
  { Initialize parser state }
  FHasError := False;
  FError := '';

  // Clear previous results to avoid memory leaks from prior runs
  for i := 0 to High(FResults) do
    Finalize(FResults[i].Value);
  SetLength(FResults, 0);

  Initialize(Value); // Ensure Value is initialized before the loop

  try
    { Step 1: Process command-line arguments }
    i := Low(Args);
    while i <= High(Args) do
    begin
      CurrentOpt := Args[i];
      
      // Handle combined short option with value (e.g., -fvalue)
      if (Length(CurrentOpt) > 2) and (CurrentOpt[1] = '-') and (CurrentOpt[2] <> '-') and (FindOption(CurrentOpt) = -1) then
      begin
        // Extract the option character
        OptionIdx := -1;
        // First check if this is a valid short option
        for j := 0 to High(FOptions) do
        begin
          if FOptions[j].ShortOpt = CurrentOpt[2] then
          begin
            OptionIdx := j;
            Break;
          end;
        end;
        
        // If not a valid short option, report error
        if OptionIdx = -1 then
        begin
          SetError('Unknown option: ' + CurrentOpt);
          Exit;
        end;
        
        // Found a valid short option
        Value := FOptions[OptionIdx].DefaultValue;
        ValueStr := Copy(CurrentOpt, 3, Length(CurrentOpt) - 2);
        
        // For boolean options, this format is invalid
        if FOptions[OptionIdx].ArgType = atBoolean then
        begin
          SetError('Invalid format for boolean option: ' + CurrentOpt);
          Exit;
        end;
        
        // For required string options, check if the value is empty
        if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType = atString) and (ValueStr = '') then
        begin
          SetError('Empty value not allowed for required option: ' + CurrentOpt);
          Exit;
        end;
        
        // Parse the value
        if not ParseValue(ValueStr, FOptions[OptionIdx].ArgType, Value) then
        begin
          SetError('Invalid value for option ' + CurrentOpt);
          Exit;
        end;
        
        // Execute callbacks if assigned
        if Assigned(FOptions[OptionIdx].Callback) then
          FOptions[OptionIdx].Callback(Value);
        if Assigned(FOptions[OptionIdx].CallbackClass) then
          FOptions[OptionIdx].CallbackClass(Value);
        
        // Store parsed value
        SetLength(FResults, Length(FResults) + 1);
        FResults[High(FResults)].Name := FOptions[OptionIdx].LongOpt;
        FResults[High(FResults)].Value := Value;
        
        Inc(i);
        Continue;
      end;
      
      { Step 2a: Validate argument format (must start with '-') }
      if (Length(CurrentOpt) = 0) or (CurrentOpt[1] <> '-') then
      begin
        SetError('Invalid argument format: ' + CurrentOpt);
        Exit;
      end;
      
      { Step 2b: Validate option exists in defined options }
      OptionIdx := FindOption(CurrentOpt);
      if OptionIdx = -1 then
      begin
        SetError('Unknown option: ' + CurrentOpt);
        Exit;
      end;
      
      { Initialize with default value for this option (sets ArgType) }
      Finalize(Value); // Clean up from previous iteration
      Value := FOptions[OptionIdx].DefaultValue;
      HasValue := False;
      
      { Step 2c: Parse the value based on option type }
      // For boolean options, just set to true when present
      if FOptions[OptionIdx].ArgType = atBoolean then
      begin
        Value.Bool := True;
        HasValue := True;
      end
      // For non-boolean options, check for an attached value (e.g., -fvalue)
      else if (Length(CurrentOpt) > 2) and (CurrentOpt[2] <> '-') and (FindOption(Copy(CurrentOpt, 1, 2)) <> -1) then
      begin
        ValueStr := Copy(CurrentOpt, 3, Length(CurrentOpt));
        if not ParseValue(ValueStr, FOptions[OptionIdx].ArgType, Value) then
        begin
          SetError('Invalid value for option ' + Copy(CurrentOpt, 1, 2));
          Exit;
        end;
        HasValue := True;
      end
      // For non-boolean options, check if a separate value is provided
      else if (i < High(Args)) and ((Length(Args[i+1]) = 0) or (Args[i+1][1] <> '-')) then
      begin
        ValueStr := Args[i+1];
        
        // For required string options, check if the value is empty before parsing
        if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType = atString) and (ValueStr = '') then
        begin
          SetError('Empty value not allowed for required option: ' + CurrentOpt);
          Exit;
        end;
        
        { Step 2d: Validate the value can be parsed to the expected type }
        if not ParseValue(ValueStr, FOptions[OptionIdx].ArgType, Value) then
        begin
          SetError('Invalid value for option ' + CurrentOpt);
          Exit;
        end;
        
        HasValue := True;
        Inc(i); { Skip the value }
      end;
      
      // Check if a required non-boolean option is missing its value
      if FOptions[OptionIdx].Required and (FOptions[OptionIdx].ArgType <> atBoolean) and not HasValue then
      begin
        SetError('Missing value for required option: ' + CurrentOpt);
        Exit;
      end;
      
      { Step 5: Execute callbacks if assigned }
      if Assigned(FOptions[OptionIdx].Callback) then
        FOptions[OptionIdx].Callback(Value);
      if Assigned(FOptions[OptionIdx].CallbackClass) then
        FOptions[OptionIdx].CallbackClass(Value);
      
      { Step 3: Store parsed value in FResults }
      SetLength(FResults, Length(FResults) + 1);
      FResults[High(FResults)].Name := FOptions[OptionIdx].LongOpt;
      FResults[High(FResults)].Value := Value;
      
      Inc(i);
    end;
    
    { Step 4: Check for missing required options }
    for j := 0 to High(FOptions) do
    begin
      if FOptions[j].Required then
      begin
        FoundRequired := False;
        for i := 0 to High(FResults) do
        begin
          if FResults[i].Name = FOptions[j].LongOpt then
          begin
            FoundRequired := True;
            Break;
          end;
        end;
        
        if not FoundRequired then
        begin
          SetError('Missing required option: --' + FOptions[j].LongOpt);
          Exit;
        end;
      end;
    end;
  finally
    // Finalize the local Value record to free memory if an error occurred
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
  MaxShort := 0;
  MaxLong := 0;
  for i := Low(FOptions) to High(FOptions) do
  begin
    if Length(FOptions[i].ShortOpt) > MaxShort then
      MaxShort := Length(FOptions[i].ShortOpt);
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
    Write(Space(MaxShort - Length(FOptions[i].ShortOpt)));
    Write(', --' + FOptions[i].LongOpt);
    Write(Space(MaxLong - Length(FOptions[i].LongOpt)));
    Write('  ');
    WriteLn(FOptions[i].HelpText);
  end;
end;

procedure TArgParser.ShowUsage;
begin
  WriteLn('Usage: ' + FUsage);
end;

procedure TArgParser.SetUsage(const AUsage: string);
begin
  FUsage := AUsage;
end;

procedure TArgParser.SetError(const AError: string);
begin
  FHasError := True;
  FError := AError;
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
  for i := Low(FResults) to High(FResults) do
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
  for i := Low(FResults) to High(FResults) do
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
  for i := Low(FResults) to High(FResults) do
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
  for i := Low(FResults) to High(FResults) do
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
  for i := Low(FResults) to High(FResults) do
    if (FResults[i].Name = LongOpt) and (FResults[i].Value.ArgType = atArray) then
    begin
      Result := FResults[i].Value.Arr;
      Exit;
    end;
  Result := nil;
end;

end.
