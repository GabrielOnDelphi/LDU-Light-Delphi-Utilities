UNIT dutWin64Api;

{=============================================================================================================
   Gabriel Moraru
   2021
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   WIN API ISSUES

   Looks invalid typecasts in the parameters of .Perform(), SendMessage(), PostMessage().
   This could give some false positive results.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, LightCore.SearchResult, dutBase;

TYPE
  TAgent_APISendMessage = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

  TAgent_APIPerform = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

  TAgent_APISetWindowLong = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;


IMPLEMENTATION

USES
   LightCore.Pascal, LightCore, LightCore.Time;



{=============================================================================================================
   SEND MESSAGE
   POST MESSAGE
=============================================================================================================}

class function TAgent_APISendMessage.Description: string;
begin
  Result:=
    'Searches invalid typecasts in the parameters of SendMessage() and PostMessage()...'+ CRLF +
    'We look especially for things like:'+ CRLF +
    '   SendMessage(hWnd, WM_SETTEXT, 0, Integer(@MyCharArray));'+ CRLF +
    '   SendMessage(hWnd, WM_SETTEXT, 0, LongInt(@MyCharArray));'+ CRLF +
    CRLF +
    'This tool could give some false positive results.'+ CRLF +
    CRLF +
    'Accepted code formatting: '+ CRLF +
    '  SendMessage(x, 0, 0);'+ CRLF +
    '  SendMessage(x,0,0);'+CRLF +
    CRLF +
    'If the line contains the "WPARAM" and "LPARAM" keywords, it is ignored (because it is valid).'+ CRLF +
    'Same, if the parameters are 0,0.';
end;


{  Returns true if an 32 bit typecast was found on this line of code.
   StartPos is the column from which I start the search in this line. }
function Is32bitTypecast(const Line: string; StartPos: Integer): boolean;
begin
  Result:= (PosInsensitive('LongInt(' , Line) > StartPos) or
           (PosInsensitive('Integer(' , Line) > StartPos) or
           (PosInsensitive('Cardinal(', Line) > StartPos);
end;


procedure findApiParam(Body: TStringList; const Needle: string; SearchResults: tSearchResults);
var
  sLine: string;
  iColumn: Integer;
begin
for var iLine:= 0 to Body.Count-1 do
  begin
    sLine:= Body[iLine];
    if LineIsAComment(sLine) then Continue;   // Ignore lines that start with a comment symbol:   // { (*

    iColumn:= PosInsensitive(Needle, sLine);
    if iColumn > 0 then
     begin
       // We ignore cases where the WParam/LParam are 0.
       // Ex: SendMessage(Handle,CM_Exit,0,0);
       if Pos('0,0)' , sLine, iColumn) > 1 then Continue;
       if Pos('0, 0)', sLine, iColumn) > 1 then Continue;

       // We ignore cases where both WParam and LParam are found.
       if  (PosInsensitive('WPARAM', sLine)> 1)
       AND (PosInsensitive('LPARAM', sLine)> 1) then Continue;

       // We specifically look for 32 bit typecasts
       // Example: SendMessage(hWnd, WM_SETTEXT, 0, Integer(@MyCharArray));
       if Is32bitTypecast(sLine, iColumn)
       then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible invalid LongInt, Integer or Cardinal cast in SendMessage API function!', 'Use WParam/LParam instead!');
     end;
  end;
end;


procedure TAgent_APISendMessage.Execute(const FileName: string);
begin
  inherited Execute(FileName);

  { API signature: SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM); }
  findApiParam(TextBody, 'SendMessage(', SearchResults);
  findApiParam(TextBody, 'PostMessage(', SearchResults);

  Finalize;
end;







{=============================================================================================================
   TComponent.Perform
=============================================================================================================}
{ Signature:
       function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
       function Perform(Msg: Cardinal; WParam: WPARAM; LParam: PChar) : LRESULT;
       function Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TRect): LRESULT;  }

class function TAgent_APIPerform.Description: string;
begin
  Result:= 'Searches for invalid typecasts in the parameters of the TComponent.Perform method.'+ CRLF +
           'This tool could give some false positive results.'+ CRLF +
           'Accepted code formatting:'+ CRLF +
           '  .Perform(x, 0, 0);'+ CRLF +
           '  .Perform(x,0,0);';
end;

procedure TAgent_APIPerform.Execute(const FileName: string);
begin
  inherited Execute(FileName);

  findApiParam(TextBody, '.Perform(', SearchResults);

  Finalize; // Increment counters
end;





{=============================================================================================================
   SetWindowLong
=============================================================================================================}
class function TAgent_APISetWindowLong.Description: string;
begin
  Result:= 'On Win64, SetWindowLong and GetWindowLong must be replaced with SetWindowLongPtr/GetWindowLongPtr'+ CRLF+
           ' for GWLP_HINSTANCE, GWLP_ID, GWLP_USERDATA, GWLP_HWNDPARENT and GWLP_WNDPROC as they return pointers and handles.'+ CRLF+
           ' Pointers that are passed to SetWindowLongPtr should be type-casted to LONG_PTR and not to Integer/Longint.'+ CRLF+
           CRLF+
           'Wrong:'+ CRLF+
           '    SetWindowLong(hWnd, GWL_WNDPROC, Longint(@MyWindowProc));'+ CRLF+

           'Correct:'+ CRLF+
           '    SetWindowLongPtr(hWnd, GWLP_WNDPROC, LONG_PTR(@MyWindowProc));';
end;

procedure TAgent_APISetWindowLong.Execute(const FileName: string);
var
   sLine: string;
   iLine: Integer;
begin
  inherited Execute(FileName);

  for iLine:= 0 to TextBody.Count-1 do
    begin
      sLine:= TextBody[iLine];
      var iColumn:= RelaxedSearchI(sLine, 'etWindowLong(');
      if iColumn > 0
      then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'GetWindowLong is for Win32 only!', 'Use GetWindowLongPtr instead.');
    end;

  Finalize; // Increment counters. Save
end;









class function TAgent_APISendMessage.AgentName: string;
begin
  Result:= 'Fix SendMessage/PostMessage';
end;

class function TAgent_APIPerform.AgentName: string;
begin
  Result:= 'Fix .Perform';
end;

class function TAgent_APISetWindowLong.AgentName: string;
begin
  Result:= 'Fix SetWindowLong';
end;

end.
