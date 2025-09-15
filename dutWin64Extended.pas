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
  System.SysUtils, System.Classes, LightCore.SearchResult, dutBase;

TYPE
  // Extended
  TAgent_ExtendedPacked = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

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
    'It is recommended to replace Extended with Double.' +CRLF+
    'Details: '+ CRLF+
    '  On Win32, the size of System.Extended type is 10 bytes.'+ CRLF+
    '  However, on Win64, the System.Extended type is an alias for System.Double, which is only 8 bytes!'+ CRLF+
    '  There is no 10-byte equivalent for Extended on 64-bit.'+ CRLF+
    '  Under normal circumstances, this does not create huge problems, except for a sligly precision degradation.';
end;


procedure TAgent_Extended.Execute(const FileName: string);
var
  iColumn, iLine: Integer;
  sLine: String;
begin
  inherited Execute(FileName);

  for iLine := 0 to TextBody.Count - 1 do
  begin
    // Skip commented lines
    sLine:= TextBody[iLine];
    if LineIsAComment(sLine) then Continue;

    // Find Extended type declarations
    iColumn := PosInsensitive('Extended', sLine);
    if iColumn > 0 then
      // Check that it's not a part of a longer word (e.g., "ExtendedType")
      if (iColumn + Length('Extended') <= Length(sLine))
      AND (NOT CharIsLetter(sLine[iColumn + Length('Extended')]) )
      then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Extended type found', 'Replace Extended with Double');
  end;

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
