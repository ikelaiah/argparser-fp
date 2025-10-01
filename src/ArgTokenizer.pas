unit ArgTokenizer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, Types;




var
  // If True, small combined short flags like -abc are split into -a -b -c
  SplitCombinedShorts: Boolean = True;

type
  TArgTokenKind = (tkOption, tkPositional);

  TArgToken = record
    Kind: TArgTokenKind;
    Raw: string;      // original token text
    OptName: string;  // for options: '-x' or '--name' (empty for positionals)
    HasValue: Boolean; // whether a value was inline (via = or short-inline)
    ValueStr: string; // inline value text (for --name=value or -o=value)
  end;

  TArgTokenArray = array of TArgToken;

// Tokenize an argument list into normalized tokens.
// Rules:
// - Splits --name=value into OptName='--name' and ValueStr='value'.
// - Leaves single-dash tokens as-is (e.g., '-finput') so the caller can decide
//   if the tail is an inline value for a known short option.
// - Non-option tokens (not starting with '-') are returned as tkPositional with Raw set.
function TokenizeArgs(const Args: TStringDynArray): TArgTokenArray;

implementation

{ Helper function to check if all characters from position are letters }
function AreAllLetters(const s: string; fromPos: Integer): Boolean;
var
  j: Integer;
begin
  Result := True;
  for j := fromPos to Length(s) do
    if not (s[j] in ['A'..'Z', 'a'..'z']) then
    begin
      Result := False;
      Break;
    end;
end;

// Helper function to create a basic token
function CreateToken(Kind: TArgTokenKind; const Raw: string): TArgToken;
begin
  Result.Kind := Kind;
  Result.Raw := Raw;
  Result.OptName := '';
  Result.HasValue := False;
  Result.ValueStr := '';
end;

// Helper function to create an option token
function CreateOptionToken(const Raw, OptName: string): TArgToken;
begin
  Result := CreateToken(tkOption, Raw);
  Result.OptName := OptName;
end;

// Helper function to create a positional token
function CreatePositionalToken(const Raw: string): TArgToken;
begin
  Result := CreateToken(tkPositional, Raw);
  Result.ValueStr := Raw;
end;

// Helper function to create an option token with inline value
function CreateOptionTokenWithValue(const Raw, OptName, ValueStr: string): TArgToken;
begin
  Result := CreateOptionToken(Raw, OptName);
  Result.HasValue := True;
  Result.ValueStr := ValueStr;
end;

// Helper function to append a token to the result array
procedure AppendToken(var Tokens: TArgTokenArray; const Token: TArgToken);
begin
  SetLength(Tokens, Length(Tokens) + 1);
  Tokens[High(Tokens)] := Token;
end;

// Handle PowerShell quirk: if next token starts with '.' append it to current value
function AppendDotTokenIfPresent(const Args: TStringDynArray; var i: Integer; var Value: string): Boolean;
begin
  Result := False;
  if (i < High(Args)) and (Length(Args[i+1]) > 0) and (Args[i+1][1] = '.') then
  begin
    Value := Value + Args[i+1];
    Inc(i);
    Result := True;
  end;
end;

function TokenizeArgs(const Args: TStringDynArray): TArgTokenArray;
var
  i, p: Integer;
  s: string;
  t: TArgToken;
  rem: string;
  j, n: Integer;
  allLetters: Boolean;
begin
  SetLength(Result, 0);
  if Length(Args) = 0 then Exit;
  
  i := Low(Args);
  while i <= High(Args) do
  begin
    s := Args[i];
    
    // Handle special cases: single '-' or negative numbers
    if (s = '-') or ((Length(s) > 1) and (s[1] = '-') and (s[2] in ['0'..'9'])) then
    begin
      AppendToken(Result, CreatePositionalToken(s));
      Inc(i);
      Continue;
    end
    // Handle option-like tokens
    else if (Length(s) > 0) and (s[1] = '-') then
    begin
      // Check for inline value with '='
      p := Pos('=', s);
      if p > 0 then
      begin
        t := CreateOptionTokenWithValue(s, Copy(s, 1, p-1), Copy(s, p+1, MaxInt));
        AppendToken(Result, t);
        Inc(i);
        Continue;
      end
      else
      begin
        // Handle combined short flags and short-inline values
        if (Length(s) > 2) and (s[2] <> '-') then
        begin
          n := Length(s) - 1; // number of chars after '-'
          allLetters := AreAllLetters(s, 2);
          
          // Split small letter combinations when enabled
          if SplitCombinedShorts and allLetters and (n <= 3) then
          begin
            // Split into multiple single-letter option tokens: -a -b -c
            for j := 2 to Length(s) do
            begin
              t := CreateOptionToken('-' + s[j], '-' + s[j]);
              AppendToken(Result, t);
            end;
            Inc(i);
            Continue;
          end
          else
          begin
            // Handle short option with inline value or preserve mixed tokens
            if allLetters or ((Length(s) >= 3) and (s[3] in ['A'..'Z', 'a'..'z'])) then
            begin
              // Treat as short option with inline value: '-finput' -> '-f' and 'input'
              AppendToken(Result, CreateOptionToken('-' + s[2], '-' + s[2]));
              
              rem := Copy(s, 3, MaxInt);
              // Handle PowerShell quirk
              AppendDotTokenIfPresent(Args, i, rem);
              
              AppendToken(Result, CreatePositionalToken(rem));
              Inc(i);
              Continue;
            end
            else
            begin
              // Preserve mixed or numeric combined tokens as single option
              AppendToken(Result, CreateOptionToken(s, s));
              Inc(i);
              Continue;
            end;
          end;
        end
        else
        begin
          // Standard option token
          AppendToken(Result, CreateOptionToken(s, s));
        end;
      end;
    end
    else
    begin
      // Positional argument
      AppendToken(Result, CreatePositionalToken(s));
    end;
    Inc(i);
  end;
end;

end.
