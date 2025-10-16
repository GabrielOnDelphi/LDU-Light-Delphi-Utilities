unit FormEditor;

{-------------------------------------------------------------------------------------------------------------
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Rudimentary PAS editor so the user can edit the file "in place" without opening it in the IDE
-------------------------------------------------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ActnList,
  LightCore.SearchResult, FormAgent, System.Actions;

type
  TfrmEditor = class(TLightForm)
    TimerRew     : TTimer;
    Container    : TPanel;
    mmoView      : TMemo;
    pnlBtm       : TPanel;
    lblRewind    : TLabel;
    btnSave      : TButton;
    btnOpenIDE   : TButton;
    btnPrev      : TButton;
    btnNext      : TButton;
    btnResetSearch: TButton;
    lblCurFile   : TLabel;
    lblDetails   : TLabel;
    ActionList: TActionList;
    actNext: TAction;
    actPrev: TAction;
    actLoadIDE: TAction;
    actSave: TAction;
    actResetSearch: TAction;
    procedure FormDestroy           (Sender: TObject);
    procedure TimerRewTimer         (Sender: TObject);
    procedure actNextExecute        (Sender: TObject);
    procedure actPrevExecute        (Sender: TObject);
    procedure actLoadIDEExecute     (Sender: TObject);
    procedure actResetSearchExecute (Sender: TObject);
    procedure actSaveExecute        (Sender: TObject);
    procedure mmoViewChange         (Sender: TObject);
    procedure mmoViewClick          (Sender: TObject);
    procedure mmoViewMouseDown      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    BlinkCount: Integer;
    procedure scrollToLine(LineNumber: Integer);
    procedure ShowDetails;
    procedure scrollToPos(Pos: Integer);   { We need it for the 'Next' button }
    function GetSelectedSearch: TSearchResult;
  public
    frmResults: TfrmAgentResults;
    CurSearchPos: Integer;                          { Current position in the PAS file, shown to the user. Increased or decreased every time we click Next/Prev }
    procedure ResetViewer;
    procedure LoadSearchRes(FileRec: TSearchResult);
    procedure LoadFileRaw(const FileName: string);
  end;

var
  frmEditor: TfrmEditor;

implementation {$R *.dfm}

USES
   LightCore.IO, FormOTA, LightCore.Pascal;



procedure TfrmEditor.FormDestroy(Sender: TObject);
begin
  ////SaveForm(Self); called by AppData
end;


function TfrmEditor.GetSelectedSearch: TSearchResult;
begin
  Result:= frmResults.GetSelectedSearch;
end;


procedure TfrmEditor.ResetViewer;
begin
  actResetSearchExecute(Self);
  BlinkCount:= 0;
  TimerRew.Enabled:= False;

  mmoView.OnChange:= nil;     // Don't trigger the OnChange until after we loaded a file
  mmoView.Clear;

  btnSave.Visible:= False;
 //del btnsave.Visible:= frmResults.lbxResults.ItemIndex >= 0;
  lblRewind.Visible:= False;
  lblCurFile.Caption:= '';
  lblDetails.Caption:= '';
  lblDetails.Hint:= '';
  //mmoView.Perform(EM_SCROLLCARET, 0, 0);
end;


{ Reset the search pos and scroll at the top of the document }
procedure TfrmEditor.actResetSearchExecute(Sender: TObject);
begin
  CurSearchPos      := 0;
  mmoView.SelStart  := 0;
  mmoView.SelLength := 0;
  scrollToLine(0);

  btnResetSearch.Visible:= True;
end;


{ Show the clicked search result }
procedure TfrmEditor.LoadSearchRes(FileRec: TSearchResult);
begin
  Assert(FileRec <> nil);
  ResetViewer;

  // Search setup
  btnNext.Visible:= FileRec.Count > 1;
  btnPrev.Visible:= btnNext.Visible;
  btnResetSearch.Visible:= btnNext.Visible;
  lblDetails.Visible:= True;
  btnOpenIDE.Enabled:= True;

  // Show file info
  lblCurFile.Caption:= ExtractFileName(FileRec.FileName)+ '     '+ IntToStr(FileRec.Count)+ 'x';
  lblCurFile.Hint   := FileRec.FileName;

  // Load file and rewind
  mmoView.Lines.LoadFromFile(FileRec.FileName);
  mmoView.OnChange:= mmoViewChange;  // Show the save btn only when we are in "Search Result" mode, not in "Raw Pas" mode

  //Scroll to first found pos
  scrollToPos(0);
end;


{ Show the clicked raw PAS file }
procedure TfrmEditor.LoadFileRaw(const FileName: string);
begin
  Assert(FileName <> '');
  ResetViewer;

  // Search setup
  btnNext.Visible:= False;
  btnPrev.Visible:= btnNext.Visible;
  btnResetSearch.Visible:= False;
  lblDetails.Visible:= False;


  // Show file info
  lblCurFile.Caption:= FileName;

  // Load file and rewind
  mmoView.Lines.LoadFromFile(FileName);
  // mmoView.OnChange:= nil;  // Show the save btn only when we are in "Search Result" mode, not in "Raw Pas" mode

  frmResults.mmoStats.Clear;
  frmResults.mmoStats.Lines.Add('Total lines: '+ IntToStr(mmoView.Lines.Count));
  frmResults.mmoStats.Lines.Add('Comments: '+ IntToStr(CountComments(FileName)));
end;




{-------------------------------------------------------------------------------------------------------------
   RESULTS - NEXT / PREV
-------------------------------------------------------------------------------------------------------------}
procedure TfrmEditor.scrollToLine(LineNumber: Integer);
begin
  // Ensure the line number is within bounds
  // if (LineNumber < 0) or (LineNumber >= Memo.Lines.Count) then Exit;
  // Scroll to the top of the memo
  SendMessage(mmoView.Handle, EM_LINESCROLL, 0, -mmoView.Lines.Count);
  // Scroll down to the desired line
  SendMessage(mmoView.Handle, EM_LINESCROLL, 0, LineNumber);
end;


procedure TfrmEditor.scrollToPos(Pos: Integer);
begin
  Assert(Length(GetSelectedSearch.Positions) > 0);

  var CurLine:= GetSelectedSearch.Positions[Pos].LinePos;   // Covert search record position to line number
  scrollToLine(CurLine);
  showDetails;
end;


procedure TfrmEditor.showDetails;
begin
  var Position:= GetSelectedSearch.Positions[CurSearchPos];
  var s:= 'Pos: '+ IntToStr(Position.LinePos)+ ':'+IntToStr(Position.ColumnPos);
  if Position.WarningMsg <> '' then s:= s + ' - '+ Position.Offender+ ' -> '+ Position.WarningMsg;
  lblDetails.Caption:= s;
  lblDetails.Hint:= lblDetails.Caption;
end;


procedure TfrmEditor.actLoadIDEExecute(Sender: TObject);
begin
  OpenFileInIDE(GetSelectedSearch, CurSearchPos);
end;


procedure TfrmEditor.actNextExecute(Sender: TObject);
begin
  Assert(GetSelectedSearch <> nil, 'No SearchResult assigned');
  TimerRew.Enabled:= True;

  Inc(CurSearchPos);
  if CurSearchPos > High(GetSelectedSearch.Positions)
  then CurSearchPos:= 0;

  // Scroll
  scrollToPos(CurSearchPos);
end;


procedure TfrmEditor.actPrevExecute(Sender: TObject);
begin
  Assert(GetSelectedSearch <> nil, 'No SearchResult assigned');
  TimerRew.Enabled:= True;

  Dec(CurSearchPos);
  if CurSearchPos < 0                              // If we are at the very top...
  then CurSearchPos:= GetSelectedSearch.Count-1;   // ...wrap to the last entry

  // Scroll
  scrollToPos(CurSearchPos);
end;


procedure TfrmEditor.TimerRewTimer(Sender: TObject);
begin
  lblRewind.Left:= 10000;
  lblRewind.Visible:= NOT lblRewind.Visible;
  if BlinkCount < 9
  then Inc(BlinkCount)
  else
    begin
      TimerRew.Enabled := False;
      BlinkCount:= 0;
    end;
end;




{-------------------------------------------------------------------------------------------------------------
   EDIT
-------------------------------------------------------------------------------------------------------------}
procedure TfrmEditor.mmoViewChange(Sender: TObject);
begin
  btnSave.Visible:= True;
end;


procedure TfrmEditor.mmoViewClick(Sender: TObject);
begin
 //ToDo: show cursor position
end;


procedure TfrmEditor.mmoViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 //ToDo: show cursor position
end;


procedure TfrmEditor.actSaveExecute(Sender: TObject);
begin
  Assert(GetSelectedSearch <> nil, 'No SearchResult assigned');

  LightCore.IO.BackupFileIncrement(GetSelectedSearch.FileName, '', '.bak');
  mmoView.Lines.SaveToFile(GetSelectedSearch.FileName);
end;


end.
