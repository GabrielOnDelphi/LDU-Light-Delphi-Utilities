unit FormExclude;

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  LightVcl.Common.AppDataForm;

TYPE
  TfrmExclude = class(TLightForm)
    mmoExclude: TMemo;
    Label1    : TLabel;
    pnlBottom : TPanel;
    btnApply: TButton;
    Button1: TButton;
    chkGlobal: TCheckBox;
    procedure FormCreate  (Sender: TObject);
    procedure FormDestroy (Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure chkGlobalClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  end;

function ExcludedFiles: String;
function GetExcludedFiles: TStringList;

VAR
   frmExclude: TfrmExclude;


IMPLEMENTATION {$R *.dfm}

USES
   LightCore.AppData, LightCore.TextFile, LightVcl.Common.AppData;




procedure TfrmExclude.FormCreate(Sender: TObject);
begin
  if FileExists(ExcludedFiles)
  then mmoExclude.Lines.LoadFromFile(ExcludedFiles);;
end;


procedure TfrmExclude.FormDestroy(Sender: TObject);
begin
  //
end;


procedure TfrmExclude.btnApplyClick(Sender: TObject);
begin
  mmoExclude.Lines.SaveToFile(ExcludedFiles);
end;


procedure TfrmExclude.Button1Click(Sender: TObject);
begin
  btnApplyClick(Sender);
  Close;
end;

procedure TfrmExclude.chkGlobalClick(Sender: TObject);
begin
  //ToDo: AI: we could use a class variable here.
end;





function ExcludedFiles: string;
begin
  Result:= AppData.AppDataFolder(True)+ 'ExcludedFiles.txt'
end;

function GetExcludedFiles: TStringList;
begin
  Result:= StringFromFileTSL(ExcludedFiles);
end;



end.
