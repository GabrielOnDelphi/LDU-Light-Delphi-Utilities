unit dutCodeFormat;

{=============================================================================================================
   Gabriel Moraru
   2024
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   CODE FORMAT TIGHT
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes,
  LightCore.SearchResult, dutBase;

TYPE
  TAgent_CodeFormat= class(TBaseAgent)
   public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;


IMPLEMENTATION
USES
   LightCore.TextFile, LightCore.IO, LightCore.Pascal, LightCore, LightCore.Time;


class function TAgent_CodeFormat.Description: string;
begin
  Result:= 'Format the code in a compact way. For example '' := '' is replaced by '':='''+ CRLF+
           'Check the code to see what kind of formatting will be applied and change it to your liking.';
end;


{ Returns the line(s) where the text was found }
procedure TAgent_CodeFormat.Execute;
var
   Front: string;
   Comments, sLine: string;
   iPos, i: Integer;
begin
  Front:= '';
  inherited Execute(FileName);

  for i:= 0 to TextBody.Count-1 do
    begin
      sLine:= TextBody[i];
      if LineIsAComment(sLine) then Continue;    // Ignore lines that start with a comment symbol:   // { (*
      if IsMethod(sLine) then Continue;

      (*
      // FIRST, check lines that are full comments
      // PROBLEM. Difficult to distinguish between human comments and actual code. We could say that if the line has at least one one Delphi specific character, it is a line of code. Sepcifics:   ; ( ) = if then else begin end
      if LineIsAComment(sLine) {(Trim(sLine) = '') and (Comments > '')} then
        begin
          TextBody[i]:= 'COMMENTS OUT!    '+ sLine+ Comments;
          Continue;
        end; *)

      // Then extract comments (//) for lines that are mixed, and process only the actual code. At the end put the comment back
      SeparateComments(sLine, Comments);

      //Fix:
      //ToDo: problem on lines like this: property ImageList: IList<TGUID> read FImageList;   <-------- the space after > will be removed! ToDo: add a check for "read"
      //ToDo: problem on lines like this: Something = (enOn,enOff);                           <-------- the spaces will be removed!
      //ToDo: problem on lines like this: Type CRLF TProcess = TProcess;                      <-------- the spaces will be removed!
      //ToDo: do not apply if the punctuation is inside a string. Example: if Pos('Test punctuation wrong. Wrong');  <--- the space after the dot will be removed!

      //Add new features:
      //ToDo: check for enters or other text after end.
      //ToDo: check if there is an empty line above and below {------------------------------------------}
      //ToDo: check parentheses: Assert( FParent<>nil);
      //ToDo: add emtyp line after the first "uses". Also after the last one? Yes

      sLine:= ReplaceString(sLine, ' <', '<');
      sLine:= ReplaceString(sLine, '< ', '<');

      sLine:= ReplaceString(sLine, ' >', '>');
      sLine:= ReplaceString(sLine, '> ', '>');

      sLine:= ReplaceString(sLine, ' =', '=');
      sLine:= ReplaceString(sLine, '= ', '=');

      sLine:= ReplaceString(sLine, ' ,', ',');
      sLine:= ReplaceString(sLine, ', ', ',');

      if Pos('  :=', sLine) < 1 then      // ignore code where the assignment symbol on multiple lines where arranged to the same column (I do that a lot)
        begin
          sLine:= ReplaceString(sLine, ' :=', ':=');
          sLine:= ReplaceString(sLine, ':= ', ':=');
        end;

      sLine:= ReplaceString(sLine, ' ;', ';');

      // Lower case code
      sLine:= ReplaceWholeWords(sLine, 'Raise', 'raise');
      sLine:= ReplaceWholeWords(sLine, 'True' , 'true');
      sLine:= ReplaceWholeWords(sLine, 'False', 'false');

      iPos:= PosInsensitive(' ;', sLine);
      if (iPos > 0)
      AND not IsMethod(sLine)
      then sLine:= ReplaceString(sLine, ' ;', ';');

      iPos:= PosInsensitive(' ;', sLine);
      if (iPos > 0)
      AND not IsMethod(sLine)
      then sLine:= ReplaceString(sLine, '; ', ';');

      TextBody[i]:= sLine+ Comments;
    end;

   var NewName:= IncrementFileName(SearchResults.Last.FileName, True);
   StringToFile(NewName, TextBody.Text, woOverwrite, wpAuto);

   Finalize;
end;






class function TAgent_CodeFormat.AgentName: string;
begin
  Result:= 'Format code tight';
end;

end.
