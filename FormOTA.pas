unit FormOTA;

{-------------------------------------------------------------------------------------------------------------
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   OTA - Sends a message to the IDE plugin.

   The string structure mus be like this:
      Filename=c:\projects\Packages\CubicCommonControls\ccRegistry.pas
      Line=8
      Col=12
      Comment=(* some comment *)
-------------------------------------------------------------------------------------------------------------}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls,
  LightCore.SearchResult;

type
  TfrmOTA = class(TLightForm)
    mmoOTA: TMemo;
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  end;

var
  frmOTA: TfrmOTA;

function  SendCommandToIDE(S: string): Boolean;
procedure OpenFileInIDE(const FileName: string);                     overload;
procedure OpenFileInIDE(SearchRes: TSearchResult; CurPos: Integer);  overload;


implementation {$R *.dfm}

USES
   LightVcl.Common.IO, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightCore.AppData, LightVcl.Visual.AppData;


procedure OpenFileInIDE(const FileName: string);
begin
  Assert(FileExistsMsg(FileName));

  var s:= 'FileName=' + FileName+ CRLF+
      'Line=1'+ CRLF+
      'Col=1' + CRLF+
      'Comment=';

  if SendCommandToIDE(s)
  then AppData.LogVerb ('Succesfully sent message.')
  else AppData.LogError('SendMessage to IDE plugin failed!')
end;


procedure OpenFileInIDE(SearchRes: TSearchResult; CurPos: Integer);
begin
  Assert(SearchRes <> nil);

  Assert(FileExistsMsg(SearchRes.FileName), 'File not found!'+ SearchRes.FileName);

  var s:= 'FileName=' + SearchRes.FileName+ CRLF+
      'Line='+ IntToStr(SearchRes.Positions[CurPos].LinePos+1)+ CRLF+
      'Col=' + IntToStr(SearchRes.Positions[CurPos].ColumnPos)+ CRLF+
      'Comment=';

  if SendCommandToIDE(s)
  then AppData.LogVerb ('Succesfully sent message.')
  else AppData.LogError('SendMessage to IDE plugin failed!')
end;


function SendCommandToIDE(S : string): Boolean;
var
  DataToSend : TCopyDataStruct;
  Receiver : HWND;
  Res : Integer;
begin
  Result:= False;

  { Prepare the data we want to send }
  DataToSend.dwData := 1;  // Registered unique ID for LighSaber apps

  { cbData must be the size in bytes of the WideString PLUS its null terminator }
  DataToSend.cbData := (Length(S) + 1) * SizeOf(Char); 
  DataToSend.lpData := PChar(S);                       // PChar(string) is a pointer to the string data

  { Send }
  Receiver := WinApi.Windows.FindWindow(PWideChar('TfrmOTAReceiver'), nil);
  if Receiver = 0
  then MessageError('IDE Receiver NOT found! Please install the IDEFileReceiver.DPK first! If it doesn''t work, uninstall and reinstall the package.')
  else
    begin
      Res:= SendMessage(Receiver, WM_COPYDATA, wParam(Application.Handle), LParam(@DataToSend));
      Result:= Res > 0;
      //ToDo: Winapi.Windows.ShowWindow(Receiver, SW_SHOWNORMAL);
      //ToDo: Winapi.Windows.SetForegroundWindow(Receiver);
    end;
end;


procedure TfrmOTA.btnTestClick(Sender: TObject);
begin
  if SendCommandToIDE(mmoOTA.Lines.Text)
  then AppData.LogVerb ('Succesfully sent message.')
  else AppData.LogError('SendMessage to IDE plugin failed!');
end;


end.
