unit ArgTokenizer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Types;

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
    t.Raw := s;
    t.OptName := '';
    t.HasValue := False;
    t.ValueStr := '';
    // Treat a single '-' token as a positional (stdin-like) and treat
    // tokens like '-1' (negative numbers) as positionals rather than
    // options. This avoids treating numeric values as option switches.
    if (s = '-') or ((Length(s) > 1) and (s[1] = '-') and (s[2] in ['0'..'9'])) then
    begin
      t.Kind := tkPositional;
      t.ValueStr := s;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := t;
      Inc(i);
      Continue;
    end
    else if (Length(s) > 0) and (s[1] = '-') then
    begin
      // Option-like token
      p := Pos('=', s);
      if p > 0 then
      begin
        t.Kind := tkOption;
        t.OptName := Copy(s, 1, p-1);
        t.HasValue := True;
        t.ValueStr := Copy(s, p+1, MaxInt);
        // add as single token
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := t;
        Inc(i);
        Continue;
      end
      else
      begin
  // Handle single-dash combined short flags and short-inline values
  if (Length(s) > 2) and (s[2] <> '-') then
        begin
          // number of chars after '-'
          n := Length(s) - 1;
          // consider splitting into single-letter flags when small group (<=3)
          allLetters := True;
          for j := 2 to Length(s) do
            if not (s[j] in ['A'..'Z', 'a'..'z']) then
            begin
              allLetters := False;
              Break;
            end;
          // Only split combined short flags when enabled by configuration
          if SplitCombinedShorts and allLetters and (n <= 3) then
          begin
            // split into multiple single-letter option tokens: -a -b -c
            for j := 2 to Length(s) do
            begin
              t.Kind := tkOption;
              t.OptName := '-' + s[j];
              t.HasValue := False;
              t.ValueStr := '';
              t.Raw := t.OptName;
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := t;
            end;
            Inc(i);
            Continue;
          end
          else
          begin
            // Decide whether this is a short option with an inline remainder
            // or a mixed/numeric combined token that should be preserved.
            // Treat as inline when either all letters (handled above for small groups)
            // or when the third character is a letter (e.g. '-finput').
            if allLetters or ((Length(s) >= 3) and (s[3] in ['A'..'Z', 'a'..'z'])) then
            begin
              // treat as short option with inline value: '-finput' -> '-f' and positional 'input'
              t.Kind := tkOption;
              t.OptName := '-' + s[2];
              t.HasValue := False;
              t.ValueStr := '';
              t.Raw := t.OptName;
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := t;
              // remainder becomes a positional token (value)
              rem := Copy(s, 3, MaxInt);
              // PowerShell quirk: if next token starts with '.' append it
              if (i < High(Args)) and (Length(Args[i+1]) > 0) and (Args[i+1][1] = '.') then
                rem := rem + Args[i+1];
              t.Kind := tkPositional;
              t.Raw := rem;
              t.ValueStr := rem;
              t.OptName := '';
              t.HasValue := True;
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := t;
              // advance by 1 (current) plus one more if we appended next token
              if (i < High(Args)) and (Length(Args[i+1]) > 0) and (Args[i+1][1] = '.') then
                Inc(i, 2)
              else
                Inc(i);
              Continue;
            end
            else
            begin
              // Mixed or numeric combined shorts should be preserved as a single token
              t.Kind := tkOption;
              t.OptName := s;
              t.Raw := s;
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := t;
              Inc(i);
              Continue;
            end;
          end;
        end
        else
        begin
          t.Kind := tkOption;
          t.OptName := s;
        end;
      end;
    end
    else
    begin
      t.Kind := tkPositional;
      // For positionals store the raw as ValueStr too for convenience
      t.ValueStr := s;
    end;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := t;
    Inc(i);
  end;
end;

end.
