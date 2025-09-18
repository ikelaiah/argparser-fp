unit ArgParser.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ArgParser;

type
  TTestParseArgs = class(TTestCase)
  private
    FParser: TArgParser;
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

procedure TTestParseArgs.Test01_AddOptions;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.AddInteger('c','count','Set count value',0,True);
  FParser.AddString('f','file','Input file','');
  CheckEquals(3, FParser.OptionCount, 'Should have 3 options added');
end;

procedure TTestParseArgs.Test02_ParseString;
begin
  FParser.AddString('f','file','Input file','');
  FParser.Parse(['--file', 'test.txt']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals('test.txt', FParser.GetString('file'), 'String value should be parsed correctly');
end;

procedure TTestParseArgs.Test03_ParseInteger;
begin
  FParser.AddInteger('c','count','Set count value',0);
  FParser.Parse(['--count', '42']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(42, FParser.GetInteger('count'), 'Integer value should be parsed correctly');
end;

procedure TTestParseArgs.Test04_ParseFloat;
begin
  FParser.AddFloat('p','precision','Set precision',0.0);
  FParser.Parse(['--precision', '3.14159']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(3.14159, FParser.GetFloat('precision'), 'Float value should be parsed correctly');
end;

procedure TTestParseArgs.Test05_ParseBoolean;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.Parse(['--verbose']);
  CheckEquals(False, FParser.HasError, 'No error should occur');
  CheckEquals(True, FParser.GetBoolean('verbose'), 'Boolean value should be parsed correctly');
end;

procedure TTestParseArgs.Test06_ParseArray;
var Arr: TArrayOfString;
begin
  FParser.AddArray('l','list','List of items');
  FParser.Parse(['--list', 'item1,item2,item3']);
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
  FParser.Parse([]);
  CheckEquals(True, FParser.HasError, 'Missing required option should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention missing option');
end;

procedure TTestParseArgs.Test08_ErrorHandling;
begin
  FParser.AddInteger('c','count','Set count value',0);
  FParser.Parse(['--count', 'notanumber']);
  CheckEquals(True, FParser.HasError, 'Invalid value should cause error');
  CheckTrue(Pos('count', FParser.Error) > 0, 'Error message should mention invalid value');
end;

procedure TTestParseArgs.Test09_HelpAndUsage;
begin
  FParser.AddBoolean('h','help','Show help');
  FParser.Parse(['--help']);
  CheckFalse(FParser.HasError, 'Help should not be treated as error');
end;

procedure TTestParseArgs.Test10_Callbacks;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.Parse(['--verbose']);
  CheckFalse(FParser.HasError, 'No error should occur');
  CheckTrue(FParser.GetBoolean('verbose'), 'Value should be True');
end;

procedure TTestParseArgs.Test11_MixedOptions;
var ResultArr: TArrayOfString;
begin
  FParser.AddBoolean('v','verbose','Enable verbose mode');
  FParser.AddInteger('c','count','Set count value',0);
  FParser.AddString('f','file','Input file','');
  FParser.Parse(['-v','--count','10','--file','test.txt']);
  CheckFalse(FParser.HasError, 'No error should occur');
  CheckTrue(FParser.GetBoolean('verbose'));
  CheckEquals(10, FParser.GetInteger('count'));
  CheckEquals('test.txt', FParser.GetString('file'));
end;

initialization
  RegisterTest(TTestParseArgs);
end.
