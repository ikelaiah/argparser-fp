program ComplexExample;

{$mode objfpc}{$H+}{$J-}

{ ComplexExample demonstrating ArgParser features:
  - options: boolean, string, integer, float
  - multiple occurrences (repeatable option)
  - arrays and greedy positionals (NArgs = -1)
  - leftovers handling using `--`
  - ParseKnownArgs usage
  - help and usage

  Example usage:
    .\ComplexExample.exe -v -n 3 -I include1 -I include2 --threshold 0.75 fileA fileB -- --not-parsed token
}

uses
  SysUtils, ArgParser, Types;

var
  P: TArgParser;
  files: TStringDynArray;
  includes: TStringDynArray;
  leftovers: TStringDynArray;
  i: Integer;
begin
  P.Init;
  try
    P.SetUsage('ComplexExample [options] <files>');

    // Common options
    P.AddBoolean('h','help','Show this help message and exit');
    P.AddBoolean('v','verbose','Enable verbose logging');
    P.AddString('o','output','Output file', 'out.txt');
    P.AddInteger('n','count','Number of iterations', 1);
    P.AddFloat('t','threshold','Threshold value', 0.5);

    // Repeatable include paths (allow multiple occurrences)
    P.AddString('I','include','Add an include path', '');
    P.SetAllowMultiple('include', True);

    // A switch that collects unknown tokens (ParseKnownArgs demo)
    P.AddBoolean('k','known','Only parse known args (debug)');

    // Greedy positional for files
    P.AddPositional('files', atArray, 'Input files to process', '', True, -1);

    // Parse, collect leftovers (tokens after `--` are preserved)
    P.ParseCommandLine;
    leftovers := P.Leftovers;

    // Show help first if requested
    if P.GetBoolean('help') then
    begin
      P.ShowHelp;
      Exit;
    end;

    // After help, handle parse errors
    if P.HasError then
    begin
      Writeln('Error: ', P.Error);
      P.ShowUsage;
      Exit;
    end;

    Writeln('Verbose: ', BoolToStr(P.GetBoolean('verbose'), True));
    Writeln('Output: ', P.GetString('output'));
    Writeln('Count: ', P.GetInteger('count'));
    Writeln('Threshold: ', P.GetFloat('threshold'));

    includes := P.GetAllString('include');
    Writeln('Includes (count=', Length(includes), '):');
    for i := 0 to High(includes) do
      Writeln('  ', includes[i]);

    files := P.GetArray('files');
    Writeln('Files (count=', Length(files), '):');
    for i := 0 to High(files) do
      Writeln('  ', files[i]);

    Writeln('Leftovers (count=', Length(leftovers), '):');
    for i := 0 to High(leftovers) do
      Writeln('  ', leftovers[i]);

  finally
    P.Done;
  end;
end.
