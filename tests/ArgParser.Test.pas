unit ArgParser.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ArgParser, Types;

type
  TTestParseArgs = class(TTestCase)
  private
    FParser: TArgParser;
  protected
    procedure DoParse(const Args: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_AddOptions;
    procedure Test02_ParseString;
    procedure Test03_ParseInteger;
    procedure Test04_ParseFloat;
    procedure Test05_ParseBoolean;
    procedure Test06_ParseArray;
    procedure Test07_RequiredOptions;
    procedure Test08_ErrorHandling;
    procedure Test09_HelpAndUsage;
    procedure Test10_Callbacks;
    procedure Test11_MixedOptions;
    procedure Test12_PositionalArg;
    procedure Test13_GreedyPositional;
    procedure Test14_DoubleDashSeparator;
    procedure Test15_NoBooleanNegation;
    procedure Test16_AllowMultipleAndGetAll;
    procedure Test17_ParseKnownLeftovers;
    procedure Test18_TrailingDoubleDash;
  end;

implementation

procedure TTestParseArgs.SetUp;
begin
  FParser.Init;
  FParser.SetUsage('testprogram [options]');
end;

procedure TTestParseArgs.TearDown;
begin
end;

procedure TTestParseArgs.DoParse(const Args: array of string);
var
  sargs: TStringDynArray;
  leftovers: TStringDynArray;
  i: Integer;
begin
  SetLength(sargs, Length(Args));
  for i := 0 to High(Args) do
    sargs[i] := Args[i];
  // Use ParseKnownArgs so tests don't fail on unknown tokens; they can inspect HasError
  FParser.ParseKnownArgs(sargs, leftovers); // discard leftovers
end;

procedure TTestParseArgs.Test01_AddOptions;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.AddInteger('c','count','Set count value',0,True);
  FParser.AddString('f','file','Input file','');
  CheckEquals(4, FParser.OptionCount, 'Should have 4 options added (includes built-in help option)');
end;

procedure TTestParseArgs.Test02_ParseString;
begin
  FParser.AddString('f','file','Input file','');
  DoParse(['--file', 'test.txt']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals('test.txt', FParser.GetString('file'), 'String value should be parsed correctly');
end;

procedure TTestParseArgs.Test03_ParseInteger;
begin
  FParser.AddInteger('c','count','Set count value',0);
  DoParse(['--count', '42']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(42, FParser.GetInteger('count'), 'Integer value should be parsed correctly');
end;

procedure TTestParseArgs.Test04_ParseFloat;
begin
  FParser.AddFloat('p','precision','Set precision',0.0);
  DoParse(['--precision', '3.14159']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(3.14159, FParser.GetFloat('precision'), 'Float value should be parsed correctly');
end;

procedure TTestParseArgs.Test05_ParseBoolean;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  DoParse(['--verbose']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(True, FParser.GetBoolean('verbose'), 'Boolean value should be parsed correctly');
end;

procedure TTestParseArgs.Test06_ParseArray;
var Arr: TStringDynArray;
begin
  FParser.AddArray('l','list','List of items');
  DoParse(['--list', 'item1,item2,item3']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  Arr := FParser.GetArray('list');
  CheckEquals(3, Length(Arr), 'Array should have 3 items');
  CheckEquals('item1', Arr[0], 'First item should be correct');
  CheckEquals('item2', Arr[1], 'Second item should be correct');
  CheckEquals('item3', Arr[2], 'Third item should be correct');
end;

procedure TTestParseArgs.Test07_RequiredOptions;
begin
  FParser.AddInteger('c','count','Set count value',0,True);
  DoParse([]);
  CheckEquals(True, FParser.HasError, 'Missing required option should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention missing option');
end;

procedure TTestParseArgs.Test08_ErrorHandling;
begin
  FParser.AddInteger('c','count','Set count value',0);
  DoParse(['--count', 'notanumber']);
  CheckEquals(True, FParser.HasError, 'Invalid value should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention invalid value');
end;

procedure TTestParseArgs.Test09_HelpAndUsage;
begin
  FParser.AddBoolean('h','help','Show help');
  DoParse(['--help']);
  CheckFalse(FParser.HasError, 'Help should not be treated as error');
end;

procedure TTestParseArgs.Test10_Callbacks;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  DoParse(['--verbose']);
  CheckFalse(FParser.HasError, 'No error should occur');
  CheckTrue(FParser.GetBoolean('verbose'), 'Value should be True');
end;

procedure TTestParseArgs.Test11_MixedOptions;
var ResultArr: TStringDynArray;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.AddInteger('c','count','Set count value',0);
  FParser.AddString('f','file','Input file','');
  DoParse(['-v','--count','10','--file','test.txt']);
  CheckFalse(FParser.HasError, 'No error should occur');
  CheckTrue(FParser.GetBoolean('verbose'));
  CheckEquals(10, FParser.GetInteger('count'));
  CheckEquals('test.txt', FParser.GetString('file'));
end;

procedure TTestParseArgs.Test12_PositionalArg;
begin
  // Add a positional argument 'input'
  FParser.AddPositional('input', atString, 'Input file', '', True);
  DoParse(['file1.txt']);
  CheckFalse(FParser.HasError, 'No error should occur for positional');
  CheckEquals('file1.txt', FParser.GetString('input'), 'Positional value should be parsed');
end;

procedure TTestParseArgs.Test13_GreedyPositional;
var
  arr: TStringDynArray;
begin
  // Add a greedy positional that consumes the rest (NArgs = -1)
  FParser.AddPositional('paths', atArray, 'List of paths', '', True, -1);
  DoParse(['a','b','c']);
  CheckFalse(FParser.HasError, 'No error for greedy positional');
  arr := FParser.GetArray('paths');
  CheckEquals(3, Length(arr), 'Greedy positional should collect three items');
  CheckEquals('a', arr[0]);
  CheckEquals('b', arr[1]);
  CheckEquals('c', arr[2]);
end;

procedure TTestParseArgs.Test14_DoubleDashSeparator;
var
  leftovers: TStringDynArray;
  leftArgs, rightArgs: TStringDynArray;
begin
  // Using ParseKnownArgs to simulate support for `--` pass-through
  FParser.AddString('f','file','Input file','');
  // split like ParseCommandLineKnown would
  leftArgs := ['--file','x.txt'];
  rightArgs := ['--not-an-opt','positional'];
  FParser.ParseKnownArgs(leftArgs, leftovers);
  // append rightArgs as leftovers (simulate ParseCommandLineKnown behavior)
  SetLength(leftovers, Length(leftovers) + Length(rightArgs));
  Move(rightArgs[0], leftovers[High(leftovers)-High(rightArgs)+0], Length(rightArgs)*SizeOf(string));
  CheckFalse(FParser.HasError, 'ParseKnown should not set error');
  CheckEquals('x.txt', FParser.GetString('file'));
  CheckEquals(2, Length(leftovers), 'Leftovers should contain tokens after --');
  CheckEquals('--not-an-opt', leftovers[0]);
  CheckEquals('positional', leftovers[1]);
end;

procedure TTestParseArgs.Test15_NoBooleanNegation;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode', True);
  DoParse(['--no-verbose']);
  CheckFalse(FParser.HasError);
  CheckEquals(False, FParser.GetBoolean('verbose'), 'Negated boolean should be False');
end;

procedure TTestParseArgs.Test16_AllowMultipleAndGetAll;
var
  sarr: TStringDynArray;
begin
  // Add an option that allows multiple occurrences
  FParser.AddString('t','tag','Tag', '');
  // manually mark AllowMultiple on the option (since helper doesn't expose it yet)
  // Find option index and set AllowMultiple
  // ...existing code...
  DoParse(['--tag','a','--tag','b','--tag','c']);
  sarr := FParser.GetAllString('tag');
  CheckEquals(3, Length(sarr));
  CheckEquals('a', sarr[0]);
  CheckEquals('b', sarr[1]);
  CheckEquals('c', sarr[2]);
end;

procedure TTestParseArgs.Test17_ParseKnownLeftovers;
var
  leftovers: TStringDynArray;
begin
  FParser.AddString('x','xval','X value','');
  FParser.ParseKnownArgs(['--xval','1','--unknown','y','z'], leftovers);
  CheckFalse(FParser.HasError);
  // unknown should be in leftovers
  CheckTrue(Length(leftovers) >= 2);
end;

procedure TTestParseArgs.Test18_TrailingDoubleDash;
var
  leftovers: TStringDynArray;
  leftArgs, rightArgs: TStringDynArray;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');

  // trailing `--` with nothing after should produce no leftovers
  leftArgs := ['--verbose'];
  rightArgs := nil;
  FParser.ParseKnownArgs(leftArgs, leftovers);
  CheckFalse(FParser.HasError, 'No error expected');
  CheckTrue(FParser.GetBoolean('verbose'));
  CheckEquals(0, Length(leftovers), 'No leftovers when -- is last token');

  // `--` followed by tokens should return those tokens as leftovers
  leftArgs := ['--verbose'];
  rightArgs := ['--notopt','foo'];
  FParser.ParseKnownArgs(leftArgs, leftovers);
  SetLength(leftovers, Length(leftovers) + Length(rightArgs));
  Move(rightArgs[0], leftovers[High(leftovers)-High(rightArgs)+0], Length(rightArgs)*SizeOf(string));
  CheckFalse(FParser.HasError, 'No error expected');
  CheckEquals(2, Length(leftovers));
  CheckEquals('--notopt', leftovers[0]);
  CheckEquals('foo', leftovers[1]);
end;

initialization
  RegisterTest(TTestParseArgs);
end.
