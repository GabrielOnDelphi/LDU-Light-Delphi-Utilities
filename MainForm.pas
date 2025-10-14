UNIT MainForm;

{=============================================================================================================
   Gabriel Moraru
   2021
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Description here: https://github.com/GabrielOnDelphi/DUT-My-Delphi-Utilities

   Features:
     Ignores lines that start with a comment symbol:   // { (*
-------------------------------------------------------------------------------------------------------------}
//ToDo: button to allow user so save gui

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  LightCore.AppData, LightVcl.Common.AppData, LightVcl.Common.AppDataForm, LightVcl.Visual.CheckBox, LightVcl.Common.VclUtils;
  {InternetLabel,}

TYPE
  TfrmMain = class(TLightForm)
    btnAgExtended       : TButton;
    btnAgExtendedRec    : TButton;
    btnAgFindCode       : TButton;
    btnAgIntfImpl       : TButton;
    btnAgLongInt        : TButton;
    btnAgPerform        : TButton;
    btnAgPointer        : TButton;
    btnAgSendMsgTypeCst : TButton;
    btnAgWinLong        : TButton;
    btnBOM              : TButton;
    btnBom_Ansi2Utf     : TButton;
    btnBom_Utf2Ansi     : TButton;
    btnColorPick        : TButton;
    btnFreeAndNil3      : TButton;
    btnSettings         : TButton;
    btnTryExcept4       : TButton;
    btnCrLf             : TButton;
    Button2             : TButton;
    Categories          : TCategoryPanelGroup;
    catSearch           : TCategoryPanel;
    catImprove          : TCategoryPanel;
    catText             : TCategoryPanel;
    catTools            : TCategoryPanel;
    lblDescription      : TLabel;
    Panel1              : TPanel;
    cat64bit            : TCategoryPanel;
    pnlLeft             : TPanel;
    setFocus4           : TButton;
    btnHelp2            : TButton;
    chkReopenLast       : TCubicCheckBox;
    chkHideMainForm     : TCubicCheckBox;
    procedure StartTask       (Sender: TObject);
    procedure btnHelp2Click   (Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnMouseEnter   (Sender: TObject);
    procedure btnColorPickClick(Sender: TObject);
  private
    LastAgent: Integer;   // Used to restore the last agent
  public
    procedure FormPostInitialize; override;
    procedure FormPreRelease;     override;
  end;

VAR
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}

USES
   dutAgentFactory,
   LightVcl.Common.ExecuteShell, LightVcl.Common.IniFileQuick, FormColorPicker, FormOptions, FormAgent;


{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}

procedure TfrmMain.FormPostInitialize;
var
  i, j, k: Integer;
  Panel: TCategoryPanel;
  Surface: TCategoryPanelSurface;
  Control: TControl;
  Btn: TButton;
begin
  inherited FormPostInitialize;

  if AppData.RunningFirstTime
  then ExecuteURL(AppData.ProductWelcome);

  // Settings
  AppData.CreateFormHidden(TfrmOptions, frmOptions, asFull);
  AppData.Initializing:= FALSE;

  // Hide main form
  if chkHideMainForm.Checked then Hide;

  // Select and reopen the last agent
  LastAgent:= ReadInteger('LastAgent', -1);
  if LastAgent > 0 then
    begin
      // Select the last agent
      // Iterate through all Category Panels and their components to find the button
      for i := 0 to Categories.ControlCount - 1 do
      begin
        if Categories.Controls[i] is TCategoryPanel then
        begin
          Panel := TCategoryPanel(Categories.Controls[i]);
          // Loop 2: Check the controls (which should be the TCategoryPanelSurface)
          for j := 0 to Panel.ControlCount - 1 do
          begin
            Surface := Panel.Controls[j] as TCategoryPanelSurface; // This is confirmed to be TCategoryPanelSurface
            // Loop 3: Iterate through the controls on the TCategoryPanelSurface
            // We assume the buttons are directly on the surface
            for k := 0 to Surface.ControlCount - 1 do
            begin
              Control := Surface.Controls[k];

              if (Control is TButton) then
              begin
                Btn := TButton(Control);
                if Btn.Tag = LastAgent then
                begin
                  // Setting focus and reopening
                  // Note: Panel.Expanded := True may be needed if the category starts collapsed
                  Panel.Collapsed := False; // Ensure the panel is visible before setting focus
                  LightVcl.Common.VclUtils.SetFocus(Btn);

                  // We found it, so we can stop searching and proceed to reopen
                  if chkReopenLast.Checked
                  then CreateAgentForm(LastAgent);
                  Exit; // Exit the FormPostInitialize procedure
                end;
              end;
            end;
          end;
        end;
      end;
    end;
end;


procedure TfrmMain.FormPreRelease;
begin
  WriteInteger('LastAgent', LastAgent);
end;


{-------------------------------------------------------------------------------------------------------------
   SEARCH
-------------------------------------------------------------------------------------------------------------}
procedure TfrmMain.StartTask(Sender: TObject);
begin
  Assert((Sender as TButton).Tag > 0, 'Unknown tag!');

  LastAgent:= (Sender as TButton).Tag;
  CreateAgentForm(LastAgent);
end;


procedure TfrmMain.btnMouseEnter(Sender: TObject);
begin
  VAR Tag:= (Sender as TButton).Tag;
  if Tag > 0
  then lblDescription.Caption:= TDutAgentFactory.GetAgentDescription(Tag)
  else lblDescription.Caption:= '';
end;




{-------------------------------------------------------------------------------------------------------------
   HELP & OTHER TOOLS
-------------------------------------------------------------------------------------------------------------}
procedure TfrmMain.btnHelp2Click(Sender: TObject);
begin
   ExecuteURL('https://GabrielMoraru.com/setfocus-is-broken-in-delphi/');
end;


procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
  frmOptions.Show;
end;


procedure TfrmMain.btnColorPickClick(Sender: TObject);
begin
  AppData.CreateFormModal(TfrmClrPick, asFull);
end;


end.
