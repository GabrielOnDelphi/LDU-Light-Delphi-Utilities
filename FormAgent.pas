unit FormAgent;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Mask,
  LightCore.SearchResult, dutBase, LightVcl.Visual.PathEdit, LightVcl.Common.AppDataForm;

type
  TfrmAgentResults = class(TLightForm)
    Container   : TPanel;
    splResults  : TSplitter;
    PopupMenu   : TPopupMenu;
    mnuCopyName : TMenuItem;
    mnuOpen     : TMenuItem;
    pnlFiles: TPanel;
    lbxResults  : TListBox;
    pnlRight    : TPanel;
    btnSearch   : TButton;
    btnReplace  : TButton;
    mmoStats    : TMemo;
    splVertical : TSplitter;
    chkRelaxed  : TCheckBox;
    Panel2      : TPanel;
    edtFilter   : TLabeledEdit;
    btnExclude  : TButton;
    edtPath     : TCubicPathEdit;
    btnSave     : TButton;
    lblInpOut: TLabel;
    DelaySearchFiles: TTimer;
    procedure FormDestroy        (Sender: TObject);
    procedure lbxResultsClick    (Sender: TObject);
    procedure lbxResultsDblClick (Sender: TObject);
    procedure mnuCopyNameClick   (Sender: TObject);
    procedure mnuOpenClick       (Sender: TObject);
    procedure FormKeyPress       (Sender: TObject; var Key: Char);
    procedure btnSearchClick     (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure btnReplaceClick    (Sender: TObject);
    procedure edtPathPathChanged (Sender: TObject);
    procedure btnExcludeClick    (Sender: TObject);
    procedure btnSaveClick       (Sender: TObject);
    procedure DelaySearchFilesTimer(Sender: TObject);
  private
    Searched: Boolean;
    procedure ShowEditor;
    procedure SaveStatistics;
    procedure LoadCurFile;
    procedure StartTask;
    procedure SetCaption(const Msg: string);
    procedure SetAgent(ID: integer);
    procedure DoSearch;
  public
    Agent: TBaseAgent;
    procedure Reset;
    procedure LoadFirstResult;
    function  GetSelectedSearch: TSearchResult;
    procedure HideEditor;

    procedure FormPostInitialize; override;
    procedure FormPreRelease;    override;
  end;

procedure CreateAgentForm(ID: integer);


IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.IO, LightCore, LightCore.Time, LightCore.Types, LightCore.IO, LightCore.TextFile, LightCore.AppData, LightVcl.Common.AppData, LightVcl.Visual.INIFile, LightVcl.Common.Clipboard, LightVcl.Common.ExecuteShell,
   FormOTA, FormEditor, FormOptions, FormExclude,
   dutAgentFactory;



{-------------------------------------------------------------------------------------------------------------
   PRE-CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
procedure CreateAgentForm(ID: integer);
VAR
  frmAgResults: TfrmAgentResults;
begin
  AppData.CreateForm(TfrmAgentResults, frmAgResults, TRUE, asFull);
  frmAgResults.SetAgent(ID); // SET AGENT
end;


procedure TfrmAgentResults.SetAgent(ID: integer);
VAR
  AgentClass: TAgentClass;
begin
  // Convert agent ID to agent class
  AgentClass := IDToClassName(ID);

  // Set agent
  Agent:= TDutAgentFactory.CreateAgent(AgentClass, frmOptions.chkBackup.Checked);

  Caption:= Agent.AgentName;

  // DOCKING
  (Agent as AgentClass).DockSettingsForm(pnlRight);

  // ENABLE CHECKBOXES
  chkRelaxed.Visible:= Agent.CanRelax;
  btnReplace.Visible:= Agent.CanReplace;

  // EXPLORER & FILTER
  Refresh;                         // Refresh the main form so the frmExplorer is shown in the correct position
  edtPathPathChanged(NIL);         // Read files in folder. This could take a while for large folders

  if frmExclude = NIL
  then AppData.CreateFormHidden(TfrmExclude, frmExclude)
  else frmExclude.Show;

  // EDITOR
  AppData.CreateFormHidden(TfrmEditor, frmEditor);
  frmEditor.frmResults:= Self;
  HideEditor;
end;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR/DESTRUCTOR
-------------------------------------------------------------------------------------------------------------}
procedure TfrmAgentResults.FormPostInitialize;
begin
  inherited FormPostInitialize;
  LightVcl.Visual.INIFile.LoadForm(Self);
  lblInpOut.Caption:= 'Input files:';
  pnlFiles.Align:= alClient;
end;


procedure TfrmAgentResults.FormPreRelease;
begin
end;


procedure TfrmAgentResults.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmAgentResults.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Agent);
  Application.MainForm.Show;
end;


procedure TfrmAgentResults.FormKeyPress(Sender: TObject; var Key: Char);
begin
  {KeyDown event is less safer. If the form has a drop down controls with event assigned then form will close before the actual control finishes his drop-down behavior (like lookup combobox for example). Also if the form has caFree set on Close and combobox has OnCloseUp event you could get an AV because the form is closed before the combobox closeup event is called! https://stackoverflow.com/questions/41940049/onkeypress-for-escape-closes-form-by-default }
  if Ord(key) = vk_Escape then Close;
  Assert(KeyPreview, 'In order to close with Esc we need to activate KeyPreview!');
end;





{-------------------------------------------------------------------------------------------------------------
   START TASK
-------------------------------------------------------------------------------------------------------------}
procedure TfrmAgentResults.btnSearchClick(Sender: TObject);
begin
  Agent.Replace:= FALSE;
  StartTask;
end;


procedure TfrmAgentResults.SetCaption(CONST Msg: string);
begin
  Caption:= Agent.AgentName+ ' - '+ Msg;
end;

procedure TfrmAgentResults.btnReplaceClick(Sender: TObject);
begin
  Agent.Replace:= TRUE;
  StartTask;
end;

procedure TfrmAgentResults.StartTask;
var
   CurFile: string;
   FileList: TStringList;
begin
  Reset;
  if NOT DirectoryExistMsg(edtPath.Path) then EXIT;

  Agent.Clear;
  lblInpOut.Caption:= 'Searching:';
  SetCaption('Searching:');
  btnSave.Enabled:= TRUE;
  Screen.Cursor:= crHourGlass;

  FileList:= edtPath.GetFiles(edtFilter.Text, True, True, frmExclude.mmoExclude.Lines);
  TRY
    for CurFile in FileList do
       begin
         Agent.Execute(CurFile);      // Instructs the parser that we start parsing a new file. It wil create a new TSearchResult record for it.
         if Agent.SearchResults.Last.Found
         then
           begin
             //ToDo 3: truncate file name if too long
             lbxResults.AddItem(Agent.SearchResults.Last.FileName + TAB+ ' Found at: '+ Agent.SearchResults.Last.PositionsAsString, Agent.SearchResults.Last);
             lbxResults.Refresh;
           end
         else
           // Show files that do not contain the result
           if frmOptions.chkShowAllFiles.Checked
           then lbxResults.Items.Add('Not found: '+ CurFile);
       end;

      // Show global statistics
      mmoStats.Text:= '';
      mmoStats.Lines.Add('Searched '        + IntToStr(FileList.Count)  + ' files.');
      mmoStats.Lines.Add('Found in '        + IntToStr(Agent.FoundFiles)+ ' files.');
      mmoStats.Lines.Add('Total positions: '+ IntToStr(Agent.FoundLines));

      // Load first result
      LoadFirstResult;

      SetCaption('Done. Searched '+ IntToStr(FileList.Count)+ ' files. Found in  '+ IntToStr(Agent.FoundFiles)+ ' files.');
  FINALLY
    Screen.Cursor:= crDefault;
    FreeAndNil(FileList);
  END;

  if Agent.FoundFiles > 0
  then
    begin
      lblInpOut.Caption:= 'Found files:';
      pnlFiles.Align := alTop;
      pnlFiles.Height:= Container.Height DIV 3;
      splResults.Top := pnlFiles.Top+ pnlFiles.Height+1;
      splResults.Visible:= TRUE;
    end
  else
    lblInpOut.Caption:= 'No files found.';
end;


procedure TfrmAgentResults.Reset;
begin
  lblInpOut.Caption:= '';
  mmoStats.Text    := '';
  mmoStats.Visible := FALSE;
  mmoStats.Text    := '';
  Caption          := '';
  lbxResults.Clear;
  HideEditor;
end;


procedure TfrmAgentResults.SaveStatistics;
begin
  if frmOptions.chkSaveStats.Checked then
    begin
      var sOutput:= 'Results for the batch processing:'+ CRLF;

      //ToDo: don't overwrite, just append at the end (add datetime)
      for VAR s in lbxResults.Items DO
         sOutput:= sOutput+ s+ CRLF;
      StringToFile(edtPath.Path+ 'Output Summary.txt', sOutput);
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   RESULTS
-------------------------------------------------------------------------------------------------------------}
procedure TfrmAgentResults.LoadFirstResult;
begin
  Searched:= True;

  if lbxResults.Items.Count > 0
  then
    begin
      lbxResults.ItemIndex:= 0;   // Mark the first item as "current"
      LoadCurFile;                // Load the current file
      SaveStatistics;
    end
  else
     SetCaption('No issues found.');
end;


{ Returns the object selected by the user }
function TfrmAgentResults.GetSelectedSearch: TSearchResult;
begin
  Result:= lbxResults.Items.Objects[lbxResults.ItemIndex] as TSearchResult;
end;


{ Load the clicked file }
procedure TfrmAgentResults.LoadCurFile;
begin
  if Searched
  then
    if GetSelectedSearch = NIL
    then
    else
      begin
        ShowEditor;    // Make the PAS Editor visible. Must be above frmEditor.LoadSearchRes
        frmEditor.LoadSearchRes(GetSelectedSearch);
        //SetCaption(GetSelectedSearch.FileName);
      end
  else
    if  (lbxResults.Items.Count > 0)
    and (lbxResults.ItemIndex >= 0)
    then
      begin
        ShowEditor;    // Make the PAS Editor visible. Must be above frmEditor.LoadFile
        frmEditor.LoadFileRaw(lbxResults.Items[lbxResults.ItemIndex]);
        //del SetCaption(); lbxResults.Items[lbxResults.ItemIndex];
      end
end;


procedure TfrmAgentResults.lbxResultsClick(Sender: TObject);
begin
  LoadCurFile;
end;


{ Show the clicked file in Delphi IDE }
procedure TfrmAgentResults.lbxResultsDblClick(Sender: TObject);
begin
  if Searched
  then
    if GetSelectedSearch <> NIL
    then OpenFileInIDE(GetSelectedSearch, frmEditor.SearchRecPos)
    else
  else
    if  (lbxResults.Items.Count > 0)
    and (lbxResults.ItemIndex >= 0)
    then OpenFileInIDE(lbxResults.Items[lbxResults.ItemIndex]);

  ShowEditor;
end;




procedure TfrmAgentResults.btnSaveClick(Sender: TObject);
VAR s: string;
begin
  s:= ''+Agent.Needle+''+ ' was found on these lines: '+ CRLF;

  for VAR SrcResult in Agent.SearchResults DO
    for VAR Position in SrcResult.Positions DO
     begin
       s:= s+ Trim(Position.CodeLine)+ CRLF;
     end;
  StringToFile(edtPath.Path+ 'Output Lines.txt', s);
end;





{-------------------------------------------------------------------------------------------------------------
   EDITOR
-------------------------------------------------------------------------------------------------------------}
{ Make the PAS Editor visible and anchor it properly }
procedure TfrmAgentResults.ShowEditor;
begin
  splResults.Visible:= True;
  //Container.Align:= alTop;
  //Container.Height:= 200;
  //splResults.Top:= pnlTop.Top+ pnlTop.Height {lblCurFile.Height};

  frmEditor.Container.Parent:= Container;
  frmEditor.Container.Align:= alClient;
  frmEditor.Container.Top:= 9999;

  mmoStats.Visible:= True;
  splVertical.Left:= lbxResults.Width;
end;


procedure TfrmAgentResults.HideEditor;
begin
  Container.Align:= alClient;
  splResults.Visible:= False;

  frmEditor.Container.Parent:= frmEditor;
  frmEditor.ResetViewer;
end;




{-------------------------------------------------------------------------------------------------------------
   GUI
-------------------------------------------------------------------------------------------------------------}
procedure TfrmAgentResults.mnuCopyNameClick(Sender: TObject);
begin
  StringToClipboard(GetSelectedSearch.FileName);
end;


procedure TfrmAgentResults.mnuOpenClick(Sender: TObject);
begin
  ExecuteShell(GetSelectedSearch.FileName)
end;


procedure TfrmAgentResults.edtPathPathChanged(Sender: TObject);
begin
  if AppData.Initializing then EXIT;

  // If the user type C:\ then the program will try to list all files in C:\.
  // This is incredibly slow. So we restart the timer each time the user types something.
  // We perform the search only if the user stopped typing.
  DelaySearchFiles.Enabled:= FALSE;
  DelaySearchFiles.Enabled:= TRUE;
end;


procedure TfrmAgentResults.DoSearch;
var Files, ExcludedFiles: TStringList;
begin
  Files:= NIL;
  ExcludedFiles:= NIL;
  if AppData.Initializing then EXIT;

  // Stop timer
  if DelaySearchFiles.Enabled then EXIT;

  if DirectoryExists(edtPath.Path) then
   begin
     try
       ExcludedFiles:= GetExcludedFiles;
       Files:= ListFilesOf(edtPath.Path, edtFilter.Text, True, TRUE, NIL);
       lblInpOut.caption:= 'Input files: ';
       lbxResults.Items.Assign(Files);
     finally
       FreeAndNil(Files);
       FreeAndNil(ExcludedFiles);
     end;
   end;
end;


procedure TfrmAgentResults.DelaySearchFilesTimer(Sender: TObject);
begin
  DelaySearchFiles.Enabled:= FALSE;
  DoSearch;
end;


procedure TfrmAgentResults.btnExcludeClick(Sender: TObject);
begin
  frmExclude.show;
end;


end.
