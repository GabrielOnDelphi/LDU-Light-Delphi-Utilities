UNIT dutFindInterface;

{=============================================================================================================
   2025.01
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   FIND INTERFACE IMPLEMENTATION

  Finds the class(es) that implement the specified method.
  If the method belongs to an interface, the agent will list all classes
  that implement that interface.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Math, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Mask, Vcl.ExtCtrls,
  LightVcl.Common.AppDataForm, LightCore.SearchResult, dutBase, Vcl.StdCtrls;

TYPE
  TfrmSettingsIntf = class(TLightForm)
    Container: TPanel;
    edtMethod: TLabeledEdit;
    edtIntfName: TLabeledEdit;
  public
  end;


TYPE
  TAgent_FindInterface = class(TBaseAgent)
  private
    FMethodToFind: string;
    FInterfaceName: string;
    FormSettings: TfrmSettingsIntf;

    // Helper routines
    function LineContainsMethod(const ALine, AMethodType, AMethodName: string; out AColumn: Integer): Boolean;
    function LineDeclaresInterfaceImplementor(ALineIndex: Integer): Boolean;
  public
    constructor Create(aBackupFile: Boolean); override;
    destructor Destroy; override;

    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
    procedure DockSettingsForm(HostPanel: TPanel); override;
  end;



IMPLEMENTATION {$R *.dfm}
USES
   LightCore.AppData, LightVcl.Common.AppData, LightCore.Pascal, LightCore, LightCore.Time;


class function TAgent_FindInterface.AgentName: string;
begin
  Result:= 'Find implementor';
end;


class function TAgent_FindInterface.Description: string;
begin
  Result:=
     'Finds the classes that implement the specified method.'+ CRLF+
     'If the method belongs to an interface, the agent will list all classes that implement that interface.';
end;


constructor TAgent_FindInterface.Create(aBackupFile: Boolean);
begin
  inherited Create(aBackupFile);
  AppData.CreateForm(TfrmSettingsIntf, FormSettings, FALSE, asFull);
end;


destructor TAgent_FindInterface.Destroy;
begin
  FormSettings.Container.Parent:= FormSettings; // Bring the container back
  FreeAndNil(FormSettings);
  inherited;
end;


procedure TAgent_FindInterface.DockSettingsForm(HostPanel: TPanel);
begin
  FormSettings.Container.Parent:= HostPanel;
end;


procedure TAgent_FindInterface.Execute(const FileName: string);
var
  sLine: string;
  iLine, iColumn: Integer;
  ClassNamePrefix: string;
begin
  inherited Execute(FileName);

  // Read user settings once
  FMethodToFind  := Trim(FormSettings.edtMethod.Text);
  FInterfaceName := Trim(FormSettings.edtIntfName.Text);

  if FMethodToFind = '' then
  begin
    // nothing to search for
    Exit;
  end;

  if FileExists(AppData.ExeFolder + 'taifun') then
    ClassNamePrefix := 'C'
  else
    ClassNamePrefix := 'T';

  // Loop lines
  for iLine := 0 to TextBody.Count - 1 do
  begin
    sLine := TextBody[iLine];

    if LineIsAComment(sLine) then
      Continue;

    // normalize trimmed line for method detection
    // we use LineContainsMethod which returns the column where MethodType starts
    if LineContainsMethod(sLine, 'procedure ' + ClassNamePrefix, FMethodToFind, iColumn) then
    begin
      if LineDeclaresInterfaceImplementor(iLine) then
        SearchResults.Last.AddNewPos(iLine, iColumn, sLine);
    end
    else if LineContainsMethod(sLine, 'function ' + ClassNamePrefix, FMethodToFind, iColumn) then
    begin
      if LineDeclaresInterfaceImplementor(iLine) then
        SearchResults.Last.AddNewPos(iLine, iColumn, sLine);
    end;
  end;

  Finalize; // Increment counters / finalize run
end;


// Return TRUE if the line contains MethodType (e.g. 'procedure T') and the implementation '.MethodName' occurs after it. Outputs column = position where MethodType was found (1-based). Case-insensitive.
function TAgent_FindInterface.LineContainsMethod(const ALine, AMethodType, AMethodName: string; out AColumn: Integer): Boolean;
var
  SubLine: string;
  pMethodType, pImpl: Integer;
  DotName: string;
begin
  Result := False;
  AColumn := 0;
  pMethodType := PosInsensitive(AMethodType, ALine);
  if pMethodType = 0 then
    Exit;

  // after MethodType we expect either a class-qualified implementation like "TMyClass.MyMethod(" or ".MyMethod"
  // create substring starting at the MethodType position to avoid false matches earlier in the line
  SubLine := Copy(ALine, pMethodType, Length(ALine) - pMethodType + 1);

  DotName := '.' + AMethodName;
  // search for ".MethodName("  OR ".MethodName:" OR ".MethodName;"
  pImpl := PosInsensitive(DotName + '(', SubLine);
  if pImpl = 0 then
    pImpl := PosInsensitive(DotName + ':', SubLine);
  if pImpl = 0 then
    pImpl := PosInsensitive(DotName + ';', SubLine);

  if pImpl > 0 then
  begin
    AColumn := pMethodType;
    Result := True;
  end;
end;

// Walk backwards up to 100 lines (but not below 0) and try to find a "class( ... IMyInterface ... )" declaration.
// If no interface name is provided (FInterfaceName = ''), we accept any implementor (return TRUE).
// This is a heuristic — a proper Pascal parser would be better for correctness.
function TAgent_FindInterface.LineDeclaresInterfaceImplementor(ALineIndex: Integer): Boolean;
var
  UpPoint, i: Integer;
  LineText: string;
  HasClassKeyword: Boolean;
begin
  // if user didn't request an interface name, accept immediately
  if FInterfaceName = '' then
    Exit(True);

  // clamp start of backward search
  UpPoint := Max(0, ALineIndex - 100);

  Result := False;
  for i := ALineIndex downto UpPoint do
  begin
    LineText := TextBody[i];

    // We're looking for lines that contain 'class' and the interface name.
    // Typical form: TMyClass = class(TObject, IMyInterface, IOther)
    HasClassKeyword := PosInsensitive('class', LineText) > 0;
    if HasClassKeyword and (PosInsensitive(FInterfaceName, LineText) > 0) then Exit(True);

    // Sometimes the class line and interface list are split across lines: check subsequent lines above for '(' and the interface name too
    if PosInsensitive('(', LineText) > 0 then
      if PosInsensitive(FInterfaceName, LineText) > 0 then Exit(True);
  end;
end;


class function TAgent_FindInterface.CanReplace: Boolean;
begin
  Result := FALSE;
end;

END.
