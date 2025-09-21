program MyApp;

uses
  SysUtils,
  ArgParser;

var
  Parser: TArgParser;
  i: integer;

begin
  // Initialize
  Parser.Init;
  Parser.SetUsage('MyApp [options]');

  // Add options using convenience methods
  Parser.AddInteger('c', 'count', 'Set count value', 5);
  Parser.AddBoolean('v', 'verbose', 'Enable verbose mode');
  Parser.AddString('f', 'file', 'Specify a file path', '', True); // Required
  Parser.AddArray('t', 'tags', 'Comma-separated list of tags'); // Add array option
  Parser.AddBoolean('h', 'help', 'Show this help message');


  // Parse command line arguments with one call
  Parser.ParseCommandLine;

  if Parser.HasError then
  begin
    Writeln('Error: ', Parser.Error);
    Parser.ShowUsage;
    Halt(1);
  end;


  // Show help if requested
  if Parser.GetBoolean('help') then
  begin
    Parser.ShowHelp;
    Halt(0);
  end;

  // Access parsed values
  Writeln('Count: ', Parser.GetInteger('count'));
  Writeln('File: ', Parser.GetString('file'));

  // Display array values if provided
  if Length(Parser.GetArray('tags')) > 0 then
  begin
    Write('Tags: ');
    for i := 0 to High(Parser.GetArray('tags')) do
    begin
      Write(Parser.GetArray('tags')[i]);
      if i < High(Parser.GetArray('tags')) then
        Write(', ');
    end;
    Writeln;
  end;

  if Parser.GetBoolean('verbose') then
    Writeln('Verbose mode is ON');

  Writeln('Done.');
end.
