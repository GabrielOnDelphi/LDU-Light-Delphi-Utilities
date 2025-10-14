unit FormOTAReceiver;

{ This form listen to WMCopyData to receive a string.
  When received the form passes it to the IDE.
  The first line in the string is a file name. The IDE will try to open that file.

  The form is created when the plugin is loaded into the IDE

  https://stackoverflow.com/questions/24690352
  https://en.delphipraxis.net/topic/7955-how-to-open-a-file-in-the-already-running-ide/?page=3
-------------------------------------------------------------------------------------------------------------------------}

interface

uses
  Windows, Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ToolsAPI;

type
  TfrmOTAReceiver = class(TForm)  // Note: if you change the name of the class, you need to update also the sender (of the message)
    mmo: TMemo;
  private
    EdLine: Integer;
    EdCol: Integer;
    FEditorFileName: String;
    InsertedComment: String;
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
    procedure OpenInIDEEditor;
    property EditorFileName : String read FEditorFileName write FEditorFileName;
  end;


procedure Register;

implementation {$R *.dfm}

var
  frmOTAReceiver: TfrmOTAReceiver;


procedure TfrmOTAReceiver.OpenInIDEEditor;
var
  i               : Integer;
  CharPos         : TOTACharPos;
  CursorPos       : TOTAEditPos;
  FileName        : String;
  IActionServices : IOTAActionServices;
  IEditor         : IOTAEditor;
  IEditorServices : IOTAEditorServices60;
  IEditView       : IOTAEditView;
  IEditWriter     : IOTAEditWriter;
  IModule         : IOTAModule;
  IModuleServices : IOTAModuleServices;
  InsertPos       : Longint;
  IServices       : IOTAServices;
  ISourceEditor   : IOTASourceEditor;
begin
  IServices := BorlandIDEServices as IOTAServices;
  if not Assigned(IServices) then
    begin
      ShowMessage('IOTAServices not available!');
      exit;
    end;

  IServices.QueryInterface(IOTAACtionServices, IActionServices);
  if IActionServices <> Nil then
  begin

    IServices.QueryInterface(IOTAModuleServices, IModuleServices);
    Assert(IModuleServices <> Nil);

    // Close all files open in the IDE
    // IModuleServices.CloseAll;

    if IActionServices.OpenFile(EditorFileName) then
    begin

      //  At this point, if the named file has an associated .DFM and we stopped here, the form designer would be in front of the code editor.

      IModule := IModuleServices.Modules[0];
      //  IModule is the one holding our .Pas file and its .Dfm, if any
      //  So, iterate the IModule's editors until we find the one for the .Pas file and then call .Show on it.  This will bring the code editor in front of the form editor.

      ISourceEditor := Nil;

      for i := 0 to IModule.ModuleFileCount - 1 do
      begin
        IEditor := IModule.ModuleFileEditors[i];
        FileName := IEditor.FileName;

        //Debug:
        mmo.Lines.Add('----------------------');
        mmo.Lines.Add(Format('%d %s', [i, FileName]));

        if CompareText(ExtractFileExt(IEditor.FileName), '.Pas') = 0
        then
           if ISourceEditor = Nil
           then
             begin
               IEditor.QueryInterface(IOTASourceEditor, ISourceEditor);
               IEditor.Show;
             end
           else
        else
           mmo.Lines.Add('Closing editor?');
      end;

      // Next, place the editor caret where we want it ...
      IServices.QueryInterface(IOTAEditorServices, IEditorServices);
      if IEditorServices = Nil
      then ShowMessage('No IEditorServices!')
      else
        begin
          IEditView := IEditorServices.TopView;
          Assert(IEditView <> Nil);
          CursorPos.Line := edLine;
          CursorPos.Col := edCol;
          IEditView.SetCursorPos(CursorPos);
          IEditView.MoveViewToCursor;    // Scroll the IEditView to the caret

          // Insert the comment, if any
          if InsertedComment <> '' then
          begin
            mmo.Lines.Add('Comment: '+ InsertedComment);
            Assert(ISourceEditor <> Nil);

            IEditView.ConvertPos(True, CursorPos, CharPos);
            InsertPos   := IEditView.CharPosToPos(CharPos);

            IEditWriter := ISourceEditor.CreateUnDoableWriter;
            if IEditWriter = nil
            then ShowMessage('IEditWriter is nil!')
            else
             begin
               IEditWriter.CopyTo(InsertPos);
               IEditWriter.Insert(PAnsiChar(AnsiString(InsertedComment)));
               IEditWriter := Nil;
             end;
          end;
        end;
    end;
  end;
end;



procedure TfrmOTAReceiver.WMCopyData(var Msg: TWMCopyData);
var s : string;
begin
  { We need a true copy of the data before it disappear }
  SetString(s, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData div SizeOf(Char));

  if mmo.WordWrap then
   begin
     ShowMessage('LightSaber IDE Plugin problem - The path will be truncated if mmo.WordWrap is enabled!');
     mmo.WordWrap:= False;
   end;

  mmo.Text:= s;

  // This is a nasty way to send/receive/decode the text! ToDo: send data as binary
  EditorFileName := mmo.Lines.Values['FileName'];
  edLine         := StrToIntDef(mmo.Lines.Values['Line'], 0);
  edCol          := StrToIntDef(mmo.Lines.Values['Col'], 0);
  InsertedComment:= Trim(mmo.Lines.Values['Comment']);

  if EditorFileName <> ''
  then OpenInIDEEditor;

  msg.Result := Length(mmo.Lines.Text);
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
