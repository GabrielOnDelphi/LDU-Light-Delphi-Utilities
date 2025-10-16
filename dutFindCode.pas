UNIT dutFindCode;

{=============================================================================================================
   2025.01
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   AGENT: Find Delphi code
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,
  LightVcl.Visual.AppData, LightCore.SearchResult, dutBase, LightVcl.Visual.AppDataForm, LightVcl.Visual.Memo;


TYPE
  TfrmSettingsFindCode = class(TLightForm)
    Container: TPanel;
    edtText: TLabeledEdit;
    mmoExclude: TCubicMemo;
    lblExcludedWords: TLabel;
  private
    function ExcludedFile: string;
  public
    procedure FormPostInitialize; override;
    procedure FormPreRelease; override;
  end;


TYPE
  TAgent_FindCode = class(TBaseAgent)
  private
    FormSettings: TfrmSettingsFindCode;
  public
    constructor Create(BackupFile: Boolean); override;
    destructor Destroy; override;

    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
    procedure DockSettingsForm(HostPanel: TPanel); override;
  end;



IMPLEMENTATION {$R *.dfm}
USES
   LightCore.Pascal, LightCore.AppData, LightCore, LightCore.Time;




class function TAgent_FindCode.Description: string;
begin
  Result:= 'Find all files containing the specified text.'+ CRLF+
           ' and outputs the lines to screen.';
end;


constructor TAgent_FindCode.Create(BackupFile: Boolean);
begin
  inherited Create(BackupFile);
  AppData.CreateForm(TfrmSettingsFindCode, FormSettings, FALSE, asFull);   //Freed by: TAgent_FindCode.Destroy
end;


//ToDo: add support to search multiple strings sepparated by [OR] as in google.
procedure TAgent_FindCode.Execute(const FileName: string);
var
   sLine: string;
   Word: string;
   iLine: Integer;
   ExcludedWordFound: Boolean;
begin
  inherited Execute(FileName);

  Needle:= FormSettings.edtText.Text;

  if NOT CaseSensitive
  then Needle:= LowerCase(Needle);

  for iLine:= 0 to TextBody.Count-1 do
    begin
      sLine:= LowerCase(TextBody[iLine]);
      var iColumn:= Pos(Needle, sLine);
      if iColumn > 0 then
        begin
         // Exclude words in mmoExclude
         ExcludedWordFound:= FALSE;
         for Word in FormSettings.mmoExclude.Lines DO
           begin
             if Pos(LowerCase(Word), sLine) > 0 then
               begin
                 ExcludedWordFound:= TRUE;
                 Break;
               end;
           end;
          if NOT ExcludedWordFound
          then SearchResults.Last.AddNewPos(iLine, iColumn, sLine);
        end;
    end;     
	
  // Finalize handles internal counters and cleanup.
  Finalize;
end;


class function TAgent_FindCode.CanReplace: Boolean;
begin
  Result:= FALSE;
end;




{ FORM }
procedure TAgent_FindCode.DockSettingsForm(HostPanel: TPanel);
begin
  FormSettings.Container.Parent:= HostPanel;
end;


destructor TAgent_FindCode.Destroy;
begin
  FormSettings.Container.Parent:= FormSettings; // Bring the container back
  FreeAndNil(FormSettings);
  inherited;
end;


class function TAgent_FindCode.AgentName: string;
begin
  Result:= 'Find Delphi code';
end;



{ TfrmSettingsFindCode }
function TfrmSettingsFindCode.ExcludedFile: string;
begin
  Result:= AppData.AppDataFolder+ 'FindCode - Excluded words.txt';
end;


procedure TfrmSettingsFindCode.FormPostInitialize;
begin
  inherited;
  if FileExists(ExcludedFile)
  then mmoExclude.Lines.LoadFromFile(ExcludedFile);
end;


procedure TfrmSettingsFindCode.FormPreRelease;
begin
  mmoExclude.Lines.SaveToFile(ExcludedFile);
  inherited;
end;


end.
