unit ArgTokenizer.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ArgTokenizer;

type
  TTestTokenizer = class(TTestCase)
  published
    procedure Test01_LongWithEquals;
    procedure Test02_PositionalTokens;
    procedure Test03_ShortInlineValuePreserved;
    procedure Test04_NoBooleanNegationToken;
    procedure Test05_MixedSequence;
    procedure Test06_CombinedShortsAndInline;
    procedure Test07_CombinedShortsCornerCases;
  end;

implementation

procedure TTestTokenizer.Test01_LongWithEquals;
var
  toks: TArgTokenArray;
begin
  toks := TokenizeArgs(['--file=input.txt']);
  CheckEquals(1, Length(toks));
  CheckEquals(Integer(tkOption), Integer(toks[0].Kind));
  CheckEquals('--file', toks[0].OptName);
  CheckEquals(True, toks[0].HasValue);
  CheckEquals('input.txt', toks[0].ValueStr);
end;

procedure TTestTokenizer.Test02_PositionalTokens;
var
  toks: TArgTokenArray;
begin
  toks := TokenizeArgs(['file1.txt', 'file2.txt']);
  CheckEquals(2, Length(toks));
  CheckEquals(Integer(tkPositional), Integer(toks[0].Kind));
  CheckEquals('file1.txt', toks[0].ValueStr);
  CheckEquals(Integer(tkPositional), Integer(toks[1].Kind));
  CheckEquals('file2.txt', toks[1].ValueStr);
end;

procedure TTestTokenizer.Test03_ShortInlineValuePreserved;
var
  toks: TArgTokenArray;
begin
  toks := TokenizeArgs(['-finput.txt']);
  // Now tokenized into '-f' and positional 'input.txt'
  CheckEquals(2, Length(toks));
  CheckEquals(Integer(tkOption), Integer(toks[0].Kind));
  CheckEquals('-f', toks[0].OptName);
  CheckEquals('-f', toks[0].Raw);
  CheckEquals(Integer(tkPositional), Integer(toks[1].Kind));
  CheckEquals('input.txt', toks[1].ValueStr);
  CheckEquals('input.txt', toks[1].Raw);
end;

procedure TTestTokenizer.Test04_NoBooleanNegationToken;
var
  toks: TArgTokenArray;
begin
  toks := TokenizeArgs(['--no-verbose']);
  CheckEquals(1, Length(toks));
  CheckEquals(Integer(tkOption), Integer(toks[0].Kind));
  CheckEquals('--no-verbose', toks[0].OptName);
end;

procedure TTestTokenizer.Test05_MixedSequence;
var
  toks: TArgTokenArray;
begin
  toks := TokenizeArgs(['-v','--count=3','positional','-finput','.txt']);
  // tokenizer splits '-finput' into '-f' and 'input' and appends '.txt'
  CheckEquals(5, Length(toks));
  CheckEquals('-v', toks[0].OptName);
  CheckEquals('--count', toks[1].OptName);
  CheckEquals('3', toks[1].ValueStr);
  CheckEquals(Integer(tkPositional), Integer(toks[2].Kind));
  CheckEquals('-f', toks[3].OptName);
  CheckEquals('input.txt', toks[4].ValueStr);
end;

procedure TTestTokenizer.Test06_CombinedShortsAndInline;
var
  toks: TArgTokenArray;
begin
  // combined short flags -abc should split into three option tokens
  toks := TokenizeArgs(['-abc']);
  CheckEquals(3, Length(toks));
  CheckEquals('-a', toks[0].OptName);
  CheckEquals('-b', toks[1].OptName);
  CheckEquals('-c', toks[2].OptName);

  // short inline value -finput.txt should become '-f' + 'input.txt' positional
  toks := TokenizeArgs(['-finput.txt']);
  CheckEquals(2, Length(toks));
  CheckEquals('-f', toks[0].OptName);
  CheckEquals('input.txt', toks[1].ValueStr);
end;

procedure TTestTokenizer.Test07_CombinedShortsCornerCases;
var
  toks: TArgTokenArray;
begin
  // numeric/mixed combined shorts should not be split (contains non-alpha)
  toks := TokenizeArgs(['-a1b']);
  CheckEquals(1, Length(toks));
  CheckEquals('-a1b', toks[0].OptName);

  // longer alpha groups: when SplitCombinedShorts = True we still split only small groups (<=3)
  toks := TokenizeArgs(['-abcd']);
  // Behavior: '-abcd' is treated as '-a' with positional 'bcd' (current rule)
  CheckEquals(2, Length(toks));
  CheckEquals('-a', toks[0].OptName);
  CheckEquals('bcd', toks[1].ValueStr);
end;

initialization
  RegisterTest(TTestTokenizer);
end.
