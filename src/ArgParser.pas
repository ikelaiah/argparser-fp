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
  Classes, SysUtils, Types;

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
    function ParseValue(const ValueStr: string; const ArgType: TArgType; out Value: TArgValue): Boolean;
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
    procedure AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string; const Default: Boolean = True; const Required: Boolean = False);
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
begin
  SetLength(FOptions, 0);
  FUsage := '';
  FError := '';
  FHasError := False;
  SetLength(FResults, 0);
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

function TArgParser.ParseValue(const ValueStr: string; const ArgType: TArgType; out Value: TArgValue): Boolean;
var
  StringList: TStringList;
  i: Integer;
begin
  Result := False;
  Value.ArgType := ArgType;
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
        StringList := TStringList.Create;
        try
          StringList.Delimiter := ',';
          StringList.StrictDelimiter := True;
          StringList.DelimitedText := ValueStr;
          
          SetLength(Value.Arr, StringList.Count);
          for i := 0 to StringList.Count - 1 do
            Value.Arr[i] := StringList[i];
          
          Result := True;
        finally
          StringList.Free;
        end;
      end;
  end;
end;

procedure TArgParser.Parse(const Args: TStringDynArray);
var
  i, j, OptionIdx: Integer;
  CurrentOpt: string;
  Value: TArgValue;
  HasValue: Boolean;
  RequiredOptions: array of Boolean; // Track which required options were provided
begin
  { Auto --help }
  for i := Low(Args) to High(Args) do
    if (Args[i] = '-h') or (Args[i] = '--help') then
    begin
      ShowHelp;
      Exit;
    end;
  
  FHasError := False;
  FError := '';
  SetLength(FResults, 0);
  
  // Initialize tracking for required options
  SetLength(RequiredOptions, Length(FOptions));
  for i := 0 to High(FOptions) do
    RequiredOptions[i] := False;
  
  i := Low(Args);
  while i <= High(Args) do
  begin
    CurrentOpt := Args[i];
    
    if (Length(CurrentOpt) = 0) or (CurrentOpt[1] <> '-') then
    begin
      SetError('Invalid argument format');
      Exit;
    end;
    
    OptionIdx := FindOption(CurrentOpt);
    if OptionIdx = -1 then
    begin
      SetError('Unknown option: ' + CurrentOpt);
      Exit;
    end;
    
    // Mark this option as provided if it's required
    if FOptions[OptionIdx].Required then
      RequiredOptions[OptionIdx] := True;
    
    { Initialize with default value for this option (sets ArgType) }
    Value := FOptions[OptionIdx].DefaultValue;
    HasValue := False;
    
    // For boolean options, just set to true when present
    if FOptions[OptionIdx].ArgType = atBoolean then
    begin
      Value.Bool := True;
    end
    // For non-boolean options, check if a value is provided
    else if (i < High(Args)) and ((Length(Args[i+1]) = 0) or (Args[i+1][1] <> '-')) then
    begin
      HasValue := True;
      if not ParseValue(Args[i+1], FOptions[OptionIdx].ArgType, Value) then
      begin
        SetError('Invalid value for option ' + CurrentOpt);
        Exit;
      end;
      Inc(i); { Skip the value }
    end;
    
    // Execute callbacks
    if Assigned(FOptions[OptionIdx].Callback) then
      FOptions[OptionIdx].Callback(Value);
    if Assigned(FOptions[OptionIdx].CallbackClass) then
      FOptions[OptionIdx].CallbackClass(Value);
    
    { Store parsed value }
    SetLength(FResults, Length(FResults) + 1);
    FResults[High(FResults)].Name := FOptions[OptionIdx].LongOpt;
    FResults[High(FResults)].Value := Value;
    Inc(i);
  end;
  
  { Check for required options that weren't provided }
  for j := 0 to High(FOptions) do
  begin
    if FOptions[j].Required and not RequiredOptions[j] then
    begin
      SetError('Missing required option: ' + FOptions[j].LongOpt);
      Exit;
    end;
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
  DefaultValue.Str := Default;
  DefaultValue.ArgType := atString;
  Add(ShortOpt, LongOpt, atString, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddInteger(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Integer = 0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue.Int := Default;
  DefaultValue.ArgType := atInteger;
  Add(ShortOpt, LongOpt, atInteger, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddFloat(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Double = 0.0; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue.Flt := Default;
  DefaultValue.ArgType := atFloat;
  Add(ShortOpt, LongOpt, atFloat, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddBoolean(const ShortOpt: Char; const LongOpt, HelpText: string;
  const Default: Boolean = True; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue.Bool := Default;
  DefaultValue.ArgType := atBoolean;
  Add(ShortOpt, LongOpt, atBoolean, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
begin
  DefaultValue.ArgType := atArray;
  DefaultValue.Arr := nil;
  Add(ShortOpt, LongOpt, atArray, HelpText, nil, nil, Required, DefaultValue);
end;

procedure TArgParser.AddArray(const ShortOpt: Char; const LongOpt, HelpText: string; const DefaultArr: array of string; const Required: Boolean = False);
var
  DefaultValue: TArgValue;
  i: Integer;
begin
  DefaultValue.ArgType := atArray;
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
  Result := [];
end;

end.
