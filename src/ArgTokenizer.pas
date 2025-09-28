unit ArgTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

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
begin
  SetLength(Result, 0);
  if Length(Args) = 0 then Exit;
  for i := Low(Args) to High(Args) do
  begin
    s := Args[i];
    t.Raw := s;
    t.OptName := '';
    t.HasValue := False;
    t.ValueStr := '';
    if (Length(s) > 0) and (s[1] = '-') then
    begin
      // Option-like token
      p := Pos('=', s);
      if p > 0 then
      begin
        t.Kind := tkOption;
        t.OptName := Copy(s, 1, p-1);
        t.HasValue := True;
        t.ValueStr := Copy(s, p+1, MaxInt);
      end
      else
      begin
        t.Kind := tkOption;
        t.OptName := s;
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
  end;
end;

end.
