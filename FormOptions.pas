unit FormOptions;

{-------------------------------------------------------------------------------------------------------------
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------}

interface

uses
  System.Classes, Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm, Vcl.StdCtrls;

type
  TfrmOptions = class(TLightForm)
    chkShowAllFiles: TCheckBox;
    chkBackup: TCheckBox;
    chkNewWnd: TCheckBox;
    chkSaveStats: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
  end;

var
  frmOptions: TfrmOptions;

implementation {$R *.dfm}




procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  //LoadForm(Self);
end;


procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  ////SaveForm(Self); called by AppData
end;

end.
