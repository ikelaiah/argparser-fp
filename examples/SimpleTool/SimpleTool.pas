program SimpleTool;

uses
  ArgParser;

var
  Parser: TArgParser;
  InputFile: string;
  Verbose: Boolean;

begin
  // 1. Initialize the parser
  Parser.Init;
  Parser.SetUsage('Usage: mytool [options] <input-file>');

  // 2. Define options
  Parser.AddString('i', 'input', 'Input file path', '', True);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose output');

  // 3. Parse arguments
  Parser.ParseCommandLine;

  // 4. Handle errors or help
  if Parser.HasError then
  begin
    WriteLn('Error: ', Parser.Error);
    Parser.ShowUsage;
    Exit;
  end;

  if Parser.GetBoolean('help') then
  begin
    Parser.ShowHelp;
    Exit;
  end;

  // 5. Access parsed values
  InputFile := Parser.GetString('input');
  Verbose := Parser.GetBoolean('verbose');

  WriteLn('Input File: ', InputFile);
  if Verbose then
    WriteLn('Verbose mode enabled.');
end.
