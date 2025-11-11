unit FormReceiver;

{ This form listens to the WMCopyData message comming from an external program, which transmist the filename of a Delphi file.
  When the message is received, the plugin will try to open that file in the Delphi Code Editor.

  The form is created when the plugin is loaded into the IDE

  https://stackoverflow.com/questions/24690352
  https://en.delphipraxis.net/topic/7955-how-to-open-a-file-in-the-already-running-ide/?page=3
-------------------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Windows, Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, 
  ToolsAPI, uOpenFileIDE;

TYPE
  TfrmOTAReceiver = class(TForm)  // Note: if you change the name of the class, you need to update also the sender (of the message)
  private
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  end;

procedure Register;

IMPLEMENTATION
{$R *.dfm}

var
  frmOTAReceiver: TfrmOTAReceiver;


{ This converts a string comming from the WMCopyData to a record that can be passed to uOpenFileIDE.OpenInIDEEditor.
  The structure of the message is represented by RIDEPosition }
procedure OpenFileInIDEEditor(CONST s: string);
VAR IdePosition: RIdePosition;
begin
  VAR Lines:= TStringList.Create;
  try
    Lines.Text:= s;

    IdePosition.FileName := Lines.Values['FileName'];
    IdePosition.Line     := StrToIntDef(Lines.Values['Line'], 0);
    IdePosition.Col      := StrToIntDef(Lines.Values['Col'], 0);
    IdePosition.Comment  := Trim(Lines.Values['Comment']);
  finally
    Lines.Free;
  end;

  if IdePosition.FileName <> ''
  then uOpenFileIDE.OpenInIDEEditor(IdePosition);
end;


{ Here form listens to the WMCopyData message.
  The structure of the message is represented by RIdePosition }
procedure TfrmOTAReceiver.WMCopyData(var Msg: TWMCopyData);
var sData: string;
begin
  // We need a true copy of the data before it disappear
  SetString(sData, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData div SizeOf(Char));
  OpenFileInIDEEditor(sData);
end;




procedure Register;
begin
  FreeAndNil(frmOTAReceiver);

  frmOTAReceiver := TfrmOTAReceiver.Create(Nil);
  frmOTAReceiver.Show;    // Can I send a message to a window that is invisible?  // Looks like we need to show the window before we can send messages to it.
  frmOTAReceiver.Left:= -1800;
end;


initialization


finalization
  if Assigned(frmOTAReceiver) then
   begin
     frmOTAReceiver.Close;
     FreeAndNil(frmOTAReceiver);
   end;

end.
