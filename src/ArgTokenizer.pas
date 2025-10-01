unit ArgTokenizer;

{$mode objfpc}{$H+}{$J-}

{===============================================================================
  ArgTokenizer Unit - Command-Line Argument Tokenization

  This unit provides tokenization services for command-line arguments, converting
  raw string arguments into structured tokens that can be easily processed by
  the argument parser.

  Key Concepts:
  1. Tokenization - Breaking down raw command-line strings into meaningful parts
  2. Normalization - Converting different input formats into consistent token forms
  3. Token Classification - Identifying whether tokens are options or positional arguments

  Example Tokenization Flow:
  
  Input:  ["--file=test.txt", "-abc", "value", "--", "remainder"]
  
  Step 1: Parse each argument
    "--file=test.txt" → Option token with inline value
    "-abc"           → Combined short options (split if enabled)
    "value"          → Positional token
    "--"             → Option token (separator handled by parser)
    "remainder"      → Positional token
  
  Output: [
    Token{Kind: tkOption, OptName: "--file", HasValue: true, ValueStr: "test.txt"}
    Token{Kind: tkOption, OptName: "-a", HasValue: false, ValueStr: ""}
    Token{Kind: tkOption, OptName: "-b", HasValue: false, ValueStr: ""}
    Token{Kind: tkOption, OptName: "-c", HasValue: false, ValueStr: ""}
    Token{Kind: tkPositional, OptName: "", HasValue: false, ValueStr: "value"}
    Token{Kind: tkOption, OptName: "--", HasValue: false, ValueStr: ""}
    Token{Kind: tkPositional, OptName: "", HasValue: false, ValueStr: "remainder"}
  ]
===============================================================================}

interface

uses
  Classes, Types;




var
  // Global configuration: If True, small combined short flags like -abc are split into -a -b -c
  // This affects how TokenizeArgs processes combined short options:
  //   True:  "-abc" → ["-a", "-b", "-c"] (3 separate option tokens)
  //   False: "-abc" → ["-abc"] (1 combined option token)
  SplitCombinedShorts: Boolean = True;

type
  { Token classification - determines how the parser should handle each token }
  TArgTokenKind = (
    tkOption,     // Token represents a command-line option (starts with -)
    tkPositional  // Token represents a positional argument (no leading -)
  );

  { Structured representation of a command-line argument token
    
    Token Structure Examples:
    
    1. Long option with value:    "--file=test.txt"
       ┌─────────┬──────────┬─────────┬──────────────┐
       │ Kind    │ OptName  │HasValue │ ValueStr     │
       │tkOption │"--file"  │ true    │ "test.txt"   │
       └─────────┴──────────┴─────────┴──────────────┘
    
    2. Short option flag:        "-v"  
       ┌─────────┬──────────┬─────────┬──────────────┐
       │ Kind    │ OptName  │HasValue │ ValueStr     │
       │tkOption │"-v"      │ false   │ ""           │
       └─────────┴──────────┴─────────┴──────────────┘
    
    3. Positional argument:      "filename.txt"
       ┌─────────────┬──────────┬─────────┬──────────────┐
       │ Kind        │ OptName  │HasValue │ ValueStr     │
       │tkPositional │""        │ false   │"filename.txt"│
       └─────────────┴──────────┴─────────┴──────────────┘
  }
  TArgToken = record
    Kind: TArgTokenKind;     // Classification: option vs positional
    Raw: string;             // Original token text as provided by user
    OptName: string;         // For options: normalized form ('-x' or '--name')
    HasValue: Boolean;       // True if value was provided inline (--name=value)
    ValueStr: string;        // Inline value text or positional content
  end;

  TArgTokenArray = array of TArgToken;

{===============================================================================
  Core Tokenization Function

  TokenizeArgs processes raw command-line arguments and converts them into
  structured tokens for easier parsing. 

  Tokenization Decision Tree:
  
  Input: Raw string argument
      │
      ▼
  ┌─────────────────┐
  │ Does it start   │  No   ┌─────────────────────────┐
  │ with '-'?       │ ────► │ Create positional token │
  └─────────────────┘       │ ValueStr = Raw          │
      │ Yes                 └─────────────────────────┘
      ▼
  ┌─────────────────┐
  │ Is it just '-'  │  Yes  ┌─────────────────────────┐
  │ or starts with  │ ────► │ Create positional token │
  │ '-' + digit?    │       │ (stdin or negative num) │
  └─────────────────┘       └─────────────────────────┘
      │ No
      ▼
  ┌─────────────────┐
  │ Contains '='?   │  Yes  ┌─────────────────────────┐
  │                 │ ────► │ Split on '=' and create │
  └─────────────────┘       │ option token with value │
      │ No                  └─────────────────────────┘
      ▼
  ┌─────────────────┐
  │ Length > 2 and  │  Yes  ┌─────────────────────────┐
  │ 2nd char != '-'?│ ────► │ Process combined shorts │
  │ (like "-abc")   │       │ or short with inline    │
  └─────────────────┘       └─────────────────────────┘
      │ No
      ▼
  ┌─────────────────────────┐
  │ Create standard option  │
  │ token (like "--help")   │
  └─────────────────────────┘

  Combined Shorts Processing:
  
  "-abc" → Check if all letters and ≤3 chars
      │
      ├─ Yes + SplitCombinedShorts=True → ["-a", "-b", "-c"]
      │
      └─ No or False → Check for inline value
           │
           ├─ Looks like "-finput" → ["-f", "input"]
           │
           └─ Otherwise → ["-abc"] (preserve as-is)

===============================================================================}

// Tokenize an argument list into normalized tokens.
// This is the main entry point for converting raw command-line arguments
// into structured tokens that the parser can easily process.
function TokenizeArgs(const Args: TStringDynArray): TArgTokenArray;

implementation

{===============================================================================
  Helper Functions for Token Creation and Processing
  
  These functions provide a clean API for creating different types of tokens
  and performing common operations during tokenization.
===============================================================================}

{ Character Analysis Helper
  
  Checks if all characters from a given position are alphabetic letters.
  Used to determine if a combined short option like "-abc" should be split
  or treated as a single token with inline value.
  
  Examples:
    AreAllLetters("-abc", 2) → True  (all letters after '-')
    AreAllLetters("-a1b", 2) → False (contains digit '1')
}
function AreAllLetters(const s: string; fromPos: Integer): Boolean;
var
  j: Integer;
begin
  Result := True;
  for j := fromPos to Length(s) do
    if not (s[j] in ['A'..'Z', 'a'..'z']) then
    begin
      Result := False;
      Break;
    end;
end;

{ Basic Token Factory
  
  Creates a token with default values and sets the fundamental properties.
  All other token creation functions build upon this foundation.
}
function CreateToken(Kind: TArgTokenKind; const Raw: string): TArgToken;
begin
  Result.Kind := Kind;
  Result.Raw := Raw;
  Result.OptName := '';
  Result.HasValue := False;
  Result.ValueStr := '';
end;

{ Option Token Factory
  
  Creates a token representing a command-line option (flag or setting).
  The OptName is normalized (e.g., both "-v" and "--verbose" formats).
}
function CreateOptionToken(const Raw, OptName: string): TArgToken;
begin
  Result := CreateToken(tkOption, Raw);
  Result.OptName := OptName;
end;

{ Positional Token Factory
  
  Creates a token for non-option arguments (files, values, etc.).
  The ValueStr contains the actual content for easy access.
}
function CreatePositionalToken(const Raw: string): TArgToken;
begin
  Result := CreateToken(tkPositional, Raw);
  Result.ValueStr := Raw;
end;

{ Option Token with Inline Value Factory
  
  Creates an option token that includes an inline value (from --name=value syntax).
  This is used when the option and its value are provided in a single argument.
}
function CreateOptionTokenWithValue(const Raw, OptName, ValueStr: string): TArgToken;
begin
  Result := CreateOptionToken(Raw, OptName);
  Result.HasValue := True;
  Result.ValueStr := ValueStr;
end;

{ Dynamic Array Helper
  
  Appends a token to the result array. Free Pascal doesn't have built-in
  dynamic array append, so we manage the array size manually.
}
procedure AppendToken(var Tokens: TArgTokenArray; const Token: TArgToken);
begin
  SetLength(Tokens, Length(Tokens) + 1);
  Tokens[High(Tokens)] := Token;
end;

{ PowerShell Compatibility Helper
  
  Handles a PowerShell-specific quirk where arguments like "-finput.txt"
  might be split by the shell into two tokens: "-finput" and ".txt".
  
  PowerShell Quirk Example:
    User types:   myprogram -finput.txt
    Shell passes: ["-finput", ".txt"] 
    We fix to:    "-finput" + ".txt" = "input.txt"
  
  This function checks if the next argument starts with '.' and merges it
  with the current value to reconstruct the original user intent.
}
function AppendDotTokenIfPresent(const Args: TStringDynArray; var i: Integer; var Value: string): Boolean;
begin
  Result := False;
  if (i < High(Args)) and (Length(Args[i+1]) > 0) and (Args[i+1][1] = '.') then
  begin
    Value := Value + Args[i+1];  // Merge the dot-prefixed token
    Inc(i);                      // Skip the dot token in main loop
    Result := True;
  end;
end;

function TokenizeArgs(const Args: TStringDynArray): TArgTokenArray;
var
  i, p: Integer;        // Loop counter and position finder
  s: string;            // Current argument being processed
  t: TArgToken;         // Temporary token for construction
  rem: string;          // Remainder text for inline values
  j, n: Integer;        // Loop counters for character analysis
  allLetters: Boolean;  // Flag for letter-only sequences
begin
  SetLength(Result, 0);
  if Length(Args) = 0 then Exit;
  
  {============================================================================
    Main Tokenization Loop
    
    Process each command-line argument sequentially, applying tokenization
    rules to classify and structure the input for the parser.
    
    Algorithm Overview:
    
    For Each Argument:
      1. Check for special cases (stdin '-', negative numbers)
      2. If starts with '-': Process as potential option
         a. Check for inline value (contains '=')
         b. Check for combined shorts or inline value
         c. Create appropriate option token(s)
      3. Otherwise: Create positional token
    
    Special Cases Handled:
    - Single '-' → Positional (commonly means stdin)
    - '-123' → Positional (negative number, not option)
    - '--file=value' → Option with inline value
    - '-abc' → Multiple options or single option (depending on config)
    - '-finput' → Option '-f' with inline value 'input'
  ============================================================================}
  
  i := Low(Args);
  while i <= High(Args) do
  begin
    s := Args[i];  // Current argument to process
    
    {--------------------------------------------------------------------------
      Step 1: Handle Special Cases
      
      Some tokens that start with '-' are not actually options:
      - Single '-' is often used to represent stdin in Unix conventions
      - Tokens like '-123' are negative numbers, not options
      
      Example Transformations:
        "-"     → Positional token (stdin marker)
        "-42"   → Positional token (negative number)
        "-1.5"  → Positional token (negative float)
    --------------------------------------------------------------------------}
    if (s = '-') or ((Length(s) > 1) and (s[1] = '-') and (s[2] in ['0'..'9'])) then
    begin
      AppendToken(Result, CreatePositionalToken(s));
      Inc(i);
      Continue;
    end
    
    {--------------------------------------------------------------------------
      Step 2: Process Option-Like Tokens
      
      Any token starting with '-' that isn't a special case is processed
      as a potential command-line option.
    --------------------------------------------------------------------------}
    else if (Length(s) > 0) and (s[1] = '-') then
    begin
      {------------------------------------------------------------------------
        Step 2a: Check for Inline Values (--name=value or -x=value)
        
        If the argument contains '=', split it into option name and value.
        
        Example Transformations:
          "--file=test.txt"  → Option: "--file", Value: "test.txt"
          "-o=output.log"    → Option: "-o", Value: "output.log"
          "--count=5"        → Option: "--count", Value: "5"
      ------------------------------------------------------------------------}
      p := Pos('=', s);
      if p > 0 then
      begin
        t := CreateOptionTokenWithValue(s, Copy(s, 1, p-1), Copy(s, p+1, MaxInt));
        AppendToken(Result, t);
        Inc(i);
        Continue;
      end
      else
      begin
        {----------------------------------------------------------------------
          Step 2b: Handle Combined Short Options and Short Inline Values
          
          Tokens like "-abc" can be interpreted in different ways:
          1. Combined short options: -a -b -c (if all letters, ≤3 chars)
          2. Short option with inline value: -f input (if starts with letter)
          3. Single complex option: -abc (if mixed characters or long)
          
          Decision Matrix:
          
          Input    │All Letters?│Length│Split Enabled?│ Result
          ─────────┼────────────┼──────┼──────────────┼─────────────────
          "-abc"   │    Yes     │  ≤3  │     Yes      │ ["-a","-b","-c"]
          "-abc"   │    Yes     │  ≤3  │     No       │ ["-abc"]
          "-abcd"  │    Yes     │  >3  │     Yes      │ ["-a","bcd"]
          "-a1b"   │    No      │  any │     any      │ ["-a","1b"] or ["-a1b"]
          "-finput"│    Yes     │  any │     any      │ ["-f","input"]
        ----------------------------------------------------------------------}
        if (Length(s) > 2) and (s[2] <> '-') then
        begin
          n := Length(s) - 1; // Number of characters after the initial '-'
          allLetters := AreAllLetters(s, 2);
          
          {--------------------------------------------------------------------
            Option 1: Split Small Letter Combinations
            
            For small groups of letters (≤3), split into individual options
            if splitting is enabled. This handles cases like "-abc" → "-a -b -c"
          --------------------------------------------------------------------}
          if SplitCombinedShorts and allLetters and (n <= 3) then
          begin
            // Split into multiple single-letter option tokens: -a -b -c
            for j := 2 to Length(s) do
            begin
              t := CreateOptionToken('-' + s[j], '-' + s[j]);
              AppendToken(Result, t);
            end;
            Inc(i);
            Continue;
          end
          else
          begin
            {------------------------------------------------------------------
              Option 2: Short Option with Inline Value
              
              For tokens like "-finput" or "-o/path/file", treat the first
              character after '-' as the option and the rest as an inline value.
              
              Conditions for inline value processing:
              - All characters are letters, OR
              - Third character is a letter (handles cases like "-f/path")
              
              Example Transformations:
                "-finput"     → Option: "-f", Positional: "input"
                "-o/tmp/file" → Option: "-o", Positional: "/tmp/file"
            ------------------------------------------------------------------}
            if allLetters or ((Length(s) >= 3) and (s[3] in ['A'..'Z', 'a'..'z'])) then
            begin
              // Create option token for first character
              AppendToken(Result, CreateOptionToken('-' + s[2], '-' + s[2]));
              
              // Extract remainder as inline value
              rem := Copy(s, 3, MaxInt);
              
              // Handle PowerShell dot-joining quirk
              AppendDotTokenIfPresent(Args, i, rem);
              
              // Create positional token for the inline value
              AppendToken(Result, CreatePositionalToken(rem));
              Inc(i);
              Continue;
            end
            else
            begin
              {----------------------------------------------------------------
                Option 3: Preserve Complex Tokens
                
                For mixed alphanumeric or other complex patterns, preserve
                the entire token as a single option. This handles cases where
                the token doesn't fit standard patterns.
                
                Examples:
                  "-a1b"      → Single option "-a1b"
                  "-123abc"   → Single option "-123abc"
              ----------------------------------------------------------------}
              AppendToken(Result, CreateOptionToken(s, s));
              Inc(i);
              Continue;
            end;
          end;
        end
        else
        begin
          {--------------------------------------------------------------------
            Step 2c: Standard Option Tokens
            
            Handle standard option formats that don't require special processing:
            - Long options: "--help", "--verbose"
            - Simple short options: "-h", "-v"
            - Double-dash long options: "--"
          --------------------------------------------------------------------}
          AppendToken(Result, CreateOptionToken(s, s));
        end;
      end;
    end
    else
    begin
      {------------------------------------------------------------------------
        Step 3: Handle Positional Arguments
        
        Any token that doesn't start with '-' is treated as a positional
        argument (file names, values, etc.).
        
        Examples:
          "filename.txt"  → Positional token
          "123"           → Positional token  
          "value"         → Positional token
      ------------------------------------------------------------------------}
      AppendToken(Result, CreatePositionalToken(s));
    end;
    Inc(i);
  end;
end;

end.
