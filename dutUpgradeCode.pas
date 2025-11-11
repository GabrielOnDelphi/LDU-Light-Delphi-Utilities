UNIT dutUpgradeCode;

{=============================================================================================================
   Gabriel Moraru
   2020
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Upgrade code to higher standards:
    * Find abusive usage of Try/Except (places where we swallow exceptions)
    * Fix the Embarcadero SetFocus problem.
    * Replace .Free with FreeAndNil(Object)
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, LightCore.SearchResult, dutBase;

type
  TAgent_SetFocus= class(TBaseAgent)
   private
   public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
  end;

  TAgent_TryExcept= class(TBaseAgent)
   private
    Const LogUnit= 'uMyLog';
   public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
  end;

  TAgent_FreeAndNil= class(TBaseAgent)
   private
   public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
  end;



IMPLEMENTATION

USES
   LightCore.Pascal, LightCore, LightCore.Time;



class function TAgent_TryExcept.Description: string;
begin
  Result:=
     'This agent searches for places where you swallow exceptions. You should remove such code!'+ CRLF+
     'However, if you are lazy and want to keep the code, you should at least log the exception. This agent can insert logging in such places.'+ CRLF+
     'Rules: '+ crlf+
     '  * The agent always inserts the logging code if there is nothing between Except/End'+ CRLF+
     '  * The agent will not insert the logging code if:'+ CRLF+
     '        there is a "Result:= something" line after Except.'+ CRLF+
     '        there is any other code between Except/End (but it will write to a file so you can look manually over those cases)'+ crlf+
     '        your code uses try/except to calculate a result of an opperation (that could raise an exception). Example: Result:=false"'+ CRLF+
      CRLF+
     'The logging code looks like this: DebugLog(''Exception'', ''#LogCounter);'+ CRLF+
     'You will have to implement the DebugLog procedure (however you like it).'+ CRLF+
      CRLF+
     'The agent will add the "uMyLog" unit to the USES clause. You can implment your logging system there (of course you can change the name of the usnit if you want)';
end;

procedure TAgent_TryExcept.Execute;
var
   Front: string;
   sPrev, sCurrLine, NextLine, sWarnings: string;
   iLine: Integer;
   Counter: Integer;          { Incremented each time a Try/Except is found. }

  function ResultAfterExcept: Boolean;
  begin
    Result:= Pos('Result', Trim(NextLine))= 1;
  end;

  function AddLog: string;
  begin
    Result:= CRLF + Front+ 'DebugLog(''Exception'', ''#LogCounter '+ IntToStr(Counter) +''');'
  end;

begin
  Front := '';
  FFound:= FALSE;
  inherited Execute(FileName);

  sWarnings:= SearchResults.Last.FileName+ CRLF+ 'Warnings' + CRLF+ CRLF;

  for iLine:= 1 to TextBody.Count-2 do
    begin
      sPrev    := trim(TextBody[iLine-1]);
      sCurrLine:= trim(TextBody[iLine]);
      NextLine := trim(TextBody[iLine+1]);

      if LineIsAComment(sCurrLine) then Continue;     // Ignore comments

      FFound:= IsKeyword(sCurrLine, 'except');

      if FFound
      then FFound:= RelaxedSearch(NextLine, 'end;');

      if FFound then
       begin
         SearchResults.Last.AddNewPos(iLine, 1, sCurrLine);       // Log the line(s) where the text was found

         if Replace then
          begin
            Inc(Counter);

            { Add identation }
            VAR First:= FirstNonSpace(sCurrLine);
            if First > -1
            then Front:= System.StringOfChar(' ', First-1+2); // We put a nice number of spaces (same as Except)

            // Case 4. The code uses try/except to calculate a result of an opperation but it uses a boolean variable (B) instead of "Result"
            if RelaxedSearch(sPrev, 'Result:= False;')
            then sWarnings:= sWarnings+ CRLF+ 'Use of b:=true @line'+ IntToStr(iLine);

            //Case 2. the tool always inserts the logging code if there is nothing between Except/end
            if RelaxedSearch(NextLine, 'end;')  // needed?
            then sCurrLine:= sCurrLine+ AddLog
            else
              //Case 1: the tool will not insert the logging code if there is a "result:= something" line after Except
              if ResultAfterExcept
              then
               begin
                 EmptyDummy;
                 Continue;
               end
              else
                // case 3. if there is any other code between except/and the tool does insert the logging code BUT it will write to a file so we can look manually over those cases...
                begin
                  sCurrLine:= sCurrLine+ AddLog;
                  sWarnings:= sWarnings+ CRLF+ 'Code after except @line: '+ IntToStr(iLine);
                end;

            { We store the information back }
            TextBody[iLine]:= sCurrLine;
          end;
       end;
    end;

  if FFound and Replace
  then AddUnitToUses(TextBody, LogUnit);

  Finalize; // Increment counters. Save
end;




{ See also: uUtilsFocus.pas }
class function TAgent_SetFocus.Description: string;
begin
  Result:=
     'SetFocus is broken in Embarcadero Delphi.'+ CRLF+
     '  Try to set focus on a visual control that is disabled or is invisible or simply is not focusable, and the program will crash.'+ CRLF+
     '  I created a safe alternative for Delphi’s SetFocus. This tool does the batch replace.'+ CRLF+
     CRLF+
     'The agent will add the "uUtilsFocus" unit to the USES clause. You can implment your logging system there (of course you can change the name of the usnit if you want)';
end;

procedure TAgent_SetFocus.Execute;
var
   Front: string;
   sLine: string;
   iPos, i: Integer;
CONST
   ImplementingUnit = 'uUtilsFocus';
begin
  Front := '';
  FFound:= FALSE;
  inherited Execute(FileName);

  for i:= 0 to TextBody.Count-1 do
    begin
      sLine:= TextBody[i];
      iPos:= PosInsensitive('.SetFocus;', sLine);   // We search for something like: Edit2.SetFocus;
      if iPos > 0 then
       begin
         // Ignore lines that start with a comment symbol:   // { (*
         if LineIsAComment(sLine) then Continue;

         SearchResults.Last.AddNewPos(i, iPos, sLine);                       // Returns the line(s) where the text was found

         if Replace then
          begin
            { Restore spaces }
            VAR First:= FirstNonSpace(sLine);
            if First > -1
            then Front:= System.StringOfChar(' ', First-1);

            // SetFocus() can be found in LightVcl.Common.VclUtils.pas
            TextBody[i]:= Front+ ImplementingUnit+'.SetFocus('+ ExtractObjectName(sLine)+ ');';  // We write something like SetFocus(Edit2);
            FFound:= TRUE;
          end;
       end;
    end;

  if FFound and Replace
  then AddUnitToUses(TextBody, ImplementingUnit);

  Finalize; // Increment counters. Save
end;





class function TAgent_FreeAndNil.Description: string;
begin
  Result:= 'Replaces MyObject.Free with FreeAndNil(MyObject).'+ CRLF;
end;

procedure TAgent_FreeAndNil.Execute;
var
  iPos, i: Integer;
  sLine: string;
  ObjName: string;
begin
  FFound := FALSE;
  inherited Execute(FileName);

  for i := 0 to TextBody.Count - 1 do
  begin
    sLine := TextBody[i];

    // Look for lines that contain .Free
    iPos := Pos('.Free', sLine);
    if iPos > 0 then
    begin
      // Ignore lines that start with a comment symbol: // { (*
      if LineIsAComment(sLine) then Continue;

      SearchResults.Last.AddNewPos(i, iPos, sLine); // Report the line

      if Replace then
      begin
        // Extract object name
        ObjName := Copy(sLine, 1, iPos - 1);
        ObjName := Trim(ObjName);

        // Replace .Free with FreeAndNil(Object)
        sLine := StringReplace(sLine, ObjName + '.Free', 'FreeAndNil(' + ObjName + ')', [rfReplaceAll]);

        TextBody[i] := sLine;
        FFound := TRUE;
      end;
    end;
  end;

  if FFound AND Replace
  then AddUnitToUses(TextBody, 'System.SysUtils'); // Ensure FreeAndNil is available

  Finalize; // Increment counters. Save
end;







class function TAgent_FreeAndNil.CanReplace: Boolean;
begin
  Result:= TRUE;
end;

class function TAgent_TryExcept.CanReplace: Boolean;
begin
  Result:= TRUE;
end;

class function TAgent_SetFocus.CanReplace: Boolean;
begin
  Result:= TRUE;
end;

class function TAgent_SetFocus.AgentName: string;
begin
  Result:= 'Fix .SetFocus';
end;

class function TAgent_TryExcept.AgentName: string;
begin
  Result:= 'Fix Try/Except';
end;

class function TAgent_FreeAndNil.AgentName: string;
begin
  Result:= 'Fix .Free';
end;

end.
