program PositionalExample;

{$mode objfpc}{$H+}{$J-}

{
  Improved example showing:
  - How to read actual command-line arguments (ParamStrToArray)
  - How to split arguments at `--` so everything after is treated as leftover
  - How to use a greedy positional (NArgs = -1) to capture multiple files
  - How to use ParseKnownArgs to collect unknown tokens

  Usage examples (PowerShell):
    # All tokens parsed as options/positionals
    .\PositionalExample.exe --verbose fileA fileB

    # Use `--` to pass-through tokens that should not be parsed
    .\PositionalExample.exe --verbose -- --notopt fileA
}

uses
  SysUtils, ArgParser, Types;

var
  P: TArgParser;
  Args, LeftArgs, RightArgs, leftovers: TStringDynArray;
  files: TStringDynArray;
  i, dashIdx: Integer;

begin
  P.Init;
  try
    P.SetUsage('PositionalExample [options] <files>');

    // Define a greedy positional 'files' that consumes the rest when present
    P.AddPositional('files', atArray, 'Files to process', '', True, -1);
    P.AddBoolean('v','verbose','Enable verbose mode');

    // Parse the program command-line and collect leftovers (tokens after `--` are included)
    P.ParseCommandLineKnown(leftovers);

    // Print results
    Writeln('Verbose: ', BoolToStr(P.GetBoolean('verbose'), True));
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
