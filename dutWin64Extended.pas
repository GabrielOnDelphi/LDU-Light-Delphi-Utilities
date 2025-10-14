UNIT dutWin64Extended;

{=============================================================================================================
   Gabriel Moraru
   2022
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Porting code to 64 bit.
   Checks for use of "extended"
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Character, LightCore.SearchResult, dutBase;

TYPE
  // Search for 'Extended' in packed records
  TAgent_ExtendedPacked = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

  // Search for 'Extended' everywhere in code
  TAgent_Extended = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;


IMPLEMENTATION

USES
   LightCore.Pascal, LightCore, LightCore.Time;


{ Extended in packed records }
class function TAgent_ExtendedPacked.Description: string;
begin
  Result:=
    'This agent looks for packed records that have ''Extended'' fields.'  +CRLF+
    'The "packed" keyword can indicate that the record might be saved to disk.'+ CRLF+
    'If this is the case, make sure that the size of the data remains the same, no matter if we are on Win32 or Win 64.'+ CRLF+
    'Details: '+ CRLF+
    '  On Win32, the size of System.Extended type is 10 bytes.'+ CRLF+
    '  However, on Win64, the System.Extended type is an alias for System.Double, which is only 8 bytes!'+ CRLF+
    '  There is no 10-byte equivalent for Extended on 64-bit.'+ CRLF+
    '  Under normal circumstances, this does not create huge problems, except for a sligly precision degradation.';
end;


procedure TAgent_ExtendedPacked.Execute(CONST FileName: string);
const
  MaxRecSize = 100;
var
  iColumn, iLine: Integer;
  RecordStart: Integer;
begin
  inherited Execute(FileName);

  iLine := 0;
  while iLine < TextBody.Count do
    begin
      // Search for the start of a packed record
      if PosInsensitive('packed record', TextBody[iLine]) > 0 then
      begin
        RecordStart := iLine;
        Inc(iLine); // next line

        while iLine < TextBody.Count do
        begin
          // End of record?
          if PosInsensitive('end;', TextBody[iLine]) > 0
          then Break;

          // Safety in case we don't detect end of record correctly!
          if (iLine - RecordStart) >= MaxRecSize
          then Break;  // Stop if the end of rec is not comming after 100 lines!

          // Find Extended fields
          iColumn:= PosInsensitive('Extended;', TextBody[iLine]);  //ToDo: what if I have e: extended ;     Trim all spaces before I search
          if iColumn > 0
          then SearchResults.Last.AddNewPos(iLine, iColumn, TextBody[iLine], 'Extended in packed records', 'Replace the Extended with Double');

          Inc(iLine);
        end;
      end;
      Inc(iLine);
    end;

  Finalize; // Increment counters
end;



{ Replace Extended with Double }
class function TAgent_Extended.Description: string;
begin
   Result:=
    'This agent looks for occurrences of the Extended type and reports them.'  +CRLF+
    'For 64 bit compatibility, it is recommended to replace Extended with Double.' +CRLF+
    'Details: '+ CRLF+
    '  On Win32, the size of System.Extended type is 10 bytes.'+ CRLF+
    '  However, on Win64, the System.Extended type is an alias for System.Double, which is only 8 bytes!'+ CRLF+
    '  There is no 10-byte equivalent for Extended on 64-bit!'+ CRLF+
    '  Under normal circumstances, this does not create huge problems, except for a sligly precision degradation.';
end;


procedure TAgent_Extended.Execute(const FileName: string);
const
  Token = 'EXTENDED';
var
  iLine, j, L: Integer;
  sLine, UpLine: string;
  inSingleQuote, inBraceComment, inParenStarComment: Boolean;

  function IsIdentChar(c: Char): Boolean;
  begin
    // Unicode-safe identifier check: letters, digits or underscore
    Result := (c = '_') or TCharacter.IsLetterOrDigit(c);
  end;

begin
  inherited Execute(FileName);

  // Persist comment state across lines so multi-line comments are handled correctly
  inBraceComment := False;
  inParenStarComment := False;

  for iLine := 0 to TextBody.Count - 1 do
  begin
    sLine := TextBody[iLine];
    UpLine := UpperCase(sLine);
    L := Length(sLine);

    // Quick fast-path: if whole line is a leading comment, skip it
    if LineIsAComment(sLine) and not (inBraceComment or inParenStarComment) then
      Continue;

    // single-quoted strings don't usually span lines; reset per line
    inSingleQuote := False;

    j := 1;
    while j <= L do
    begin
      // If inside a single-quoted string literal
      if inSingleQuote then
      begin
        if sLine[j] = '''' then
        begin
          // doubled '' escapes a quote
          if (j < L) and (sLine[j + 1] = '''') then
            Inc(j, 2)
          else
          begin
            inSingleQuote := False;
            Inc(j);
          end;
        end
        else
          Inc(j);
        Continue;
      end;

      // If currently inside { ... } comment (may span lines)
      if inBraceComment then
      begin
        if sLine[j] = '}' then
          inBraceComment := False;
        Inc(j);
        Continue;
      end;

      // If currently inside (* ... *) comment (may span lines)
      if inParenStarComment then
      begin
        if (sLine[j] = '*') and (j < L) and (sLine[j + 1] = ')') then
        begin
          inParenStarComment := False;
          Inc(j, 2);
        end
        else
          Inc(j);
        Continue;
      end;

      // Not inside string/comment: check for comment/string starts
      if sLine[j] = '''' then
      begin
        inSingleQuote := True;
        Inc(j);
        Continue;
      end;

      if sLine[j] = '{' then
      begin
        inBraceComment := True;
        Inc(j);
        Continue;
      end;

      if (sLine[j] = '(') and (j < L) and (sLine[j + 1] = '*') then
      begin
        inParenStarComment := True;
        Inc(j, 2);
        Continue;
      end;

      // '//' starts a line comment: rest of line is ignored
      if (sLine[j] = '/') and (j < L) and (sLine[j + 1] = '/') then
        Break;

      // Candidate token check (only if enough chars remain)
      if (j + Length(Token) - 1) <= L then
      begin
        if Copy(UpLine, j, Length(Token)) = Token then
        begin
          // check surrounding chars: both must NOT be identifier chars
          if (j = 1) or (not IsIdentChar(sLine[j - 1])) then
          begin
            if (j + Length(Token) - 1 = L) or (not IsIdentChar(sLine[j + Length(Token)])) then
            begin
              // Standalone 'Extended' token found
              SearchResults.Last.AddNewPos(iLine, j, sLine, 'Extended type found', 'Replace Extended with Double');
              // report first occurrence on the line; remove Break to report multiple
              Break;
            end;
          end;
        end;
      end;

      Inc(j);
    end; // while j
  end; // for iLine

  Finalize; // Increment counters
end;






class function TAgent_ExtendedPacked.AgentName: string;
begin
  Result:= 'Find "Extended" in packed records';
end;

class function TAgent_Extended.AgentName: string;
begin
  Result:= 'Replace "Extended"';
end;

end.
