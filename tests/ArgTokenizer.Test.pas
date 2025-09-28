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
  CheckEquals(1, Length(toks));
  CheckEquals(Integer(tkOption), Integer(toks[0].Kind));
  CheckEquals('-finput.txt', toks[0].OptName);
  CheckEquals(False, toks[0].HasValue);
  CheckEquals('', toks[0].ValueStr);
  CheckEquals('-finput.txt', toks[0].Raw);
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
  CheckEquals(5, Length(toks));
  CheckEquals('-v', toks[0].OptName);
  CheckEquals('--count', toks[1].OptName);
  CheckEquals('3', toks[1].ValueStr);
  CheckEquals(Integer(tkPositional), Integer(toks[2].Kind));
  CheckEquals('-finput', toks[3].OptName);
  CheckEquals('.txt', toks[4].Raw);
end;

initialization
  RegisterTest(TTestTokenizer);
end.
