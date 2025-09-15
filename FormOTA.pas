unit FormOTA;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.StdCtrls,
  LightCore.SearchResult;

type
  TfrmOTA = class(TLightForm)
    mmoOTA: TMemo;
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  end;

var
  frmOTA: TfrmOTA;

procedure SendCommandToIDE(S: string);
procedure OpenFileInIDE(const FileName: string);    overload;
procedure OpenFileInIDE(SearchRes: TSearchResult; CurPos: Integer);  overload;


implementation {$R *.dfm}

USES
   LightVcl.Common.IO, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;


{-------------------------------------------------------------------------------------------------------------
   OTA
--------------------------------------------------------------------------------------------------------------
   The string structure mus be like this:

   Filename=c:\My projects\Packages\CubicCommonControls\cbRegistry.pas
   Line=8
   Col=12
   Comment=(* some comment *)
-------------------------------------------------------------------------------------------------------------}

procedure OpenFileInIDE(const FileName: string);
begin
  Assert(FileExistsMsg(FileName));

  var s:= 'Filename=' + FileName+ CRLF+
      'Line='+ IntToStr(1)+ CRLF+
      'Col=1'+ IntToStr(1)+ CRLF+
      'Comment=';

  SendCommandToIDE(s);
end;


procedure OpenFileInIDE(SearchRes: TSearchResult; CurPos: Integer);
begin
  Assert(SearchRes <> nil);

  Assert(FileExistsMsg(SearchRes.FileName), 'File not found!'+ SearchRes.FileName);

  var s:= 'Filename=' + SearchRes.FileName+ CRLF+
      'Line='+ IntToStr(SearchRes.Positions[CurPos].LinePos+1)+ CRLF+
      'Col= '+ IntToStr(SearchRes.Positions[CurPos].ColumnPos)+ CRLF+
      'Comment=';

  SendCommandToIDE(s);
end;


procedure SendCommandToIDE(S : string);
var
  DataToSend : TCopyDataStruct;
  Receiver : HWND;
  Res : Integer;
begin
  { Prepare the data we want to send }
  DataToSend.dwData := 1;  // Registered unique ID for LighSaber apps
  DataToSend.cbData := Length(s) * SizeOf(Char);
  DataToSend.lpData := PChar(s);

  { Send }
  Receiver := WinApi.Windows.FindWindow(PWideChar('TfrmOTAReceiver'), nil);
  if Receiver = 0
  then MessageError('IDE Receiver NOT found! Please install the IDEFileReceiver.DPK first! If it doesn''t work, uninstall and reinstall the package.')
  else
    begin
      Res := SendMessage(Receiver, WM_COPYDATA, Wparam({Handle - here was the MainForm} nil), LParam(@DataToSend));
      if Res > 0
      then //Caption := 'Succesfully sent '+IntToStr(Res)+' characters'
      else MessageError('SendMessage failed!');

      //Winapi.Windows.ShowWindow(Receiver, SW_SHOWNORMAL);
      //Winapi.Windows.SetForegroundWindow(Receiver);
    end;
end;


procedure TfrmOTA.btnTestClick(Sender: TObject);
begin
  SendCommandToIDE(mmoOTA.Lines.Text);
end;


end.
