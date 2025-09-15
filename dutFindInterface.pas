UNIT dutFindInterface;

{=============================================================================================================
   Gabriel Moraru
   2025.01
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   FIND INTERFACE IMPLEMENTATION

   Finds the class(es) that implement the specified method
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Mask, Vcl.ExtCtrls,
  LightVcl.Common.AppDataForm, LightCore.SearchResult, dutBase, Vcl.StdCtrls;


TYPE
  TfrmSettingsIntf = class(TLightForm)
    Container: TPanel;
    edtMethod: TLabeledEdit;
    edtIntfName: TLabeledEdit;
  public
  end;   //ToDo: Remember user's input data


TYPE
  TAgent_FindInterface = class(TBaseAgent)
  private
    MethodToFind: string;
    InterfaceName: string;
    FormSettings: TfrmSettingsIntf;
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
   LightCore.AppData, LightVcl.Common.AppData
, LightCore.Pascal, LightCore, LightCore.Time;


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
  AppData.CreateForm(TfrmSettingsIntf, FormSettings, FALSE, asFull);  //Freed by: HostPanel
end;


procedure TAgent_FindInterface.Execute(const FileName: string);
var
   sLine: string;
   iColumn, iLine: Integer;
   ClassNamePrefix: string;


  { MethodType represents a procedure or function. It can be one of these: "procedure T" or "function T"}
  function FindMethod(const MethodType, MethodName: string): Boolean;
  var iPos2, iPos3: Integer;
  begin
   Result:= False;
   iColumn:= PosInsensitive(MethodType, sLine);
   if iColumn > 0 then
    begin
       //ToDo: trim spaces in sLine (SearchRelaxed?)
       iPos2:= PosInsensitive('.'+ MethodName+ '(', sLine);  // Search only full words
       iPos3:= PosInsensitive('.'+ MethodName+ ';', sLine);  // Search only full words

       const x= iColumn+ Length(MethodType)+ 3;

       if (iPos2 > x) or (iPos3 > x)
       then Result:= True;   // Returns the line(s) where the text was found
    end;
  end;

  // Once the method was found, we search backwords (from cur pos to top) 100 lines to see which class implements it
  function FindImplementor: Boolean;
  begin
     if InterfaceName = ''
     then Exit(TRUE)
     else Result:= FALSE;

     var UpPoint:= Min(0, iLine-100);
     for var j:= iLine downto UpPoint do
       if (PosInsensitive(',' + InterfaceName, TextBody[j]) > 7)
       or (PosInsensitive(', '+ InterfaceName, TextBody[j]) > 7)
       then Exit(True);
  end;

begin
  inherited Execute(FileName);

  MethodToFind  := FormSettings.edtMethod.Text;
  InterfaceName := FormSettings.edtIntfName.Text;

  if FileExists(AppData.ExeFolder+ 'taifun')
  then ClassNamePrefix:= 'C'
  else ClassNamePrefix:= 'T';

  for iLine:= 0 to TextBody.Count-1 do
    begin
      sLine:= TextBody[iLine];
      if not LineIsAComment(sLine) then    // Ignore lines that start with a comment symbol:   // { (*
       begin
         // Search for procedures
         if FindMethod('procedure '+ ClassNamePrefix, MethodToFind)     // T or C
         and FindImplementor
         then SearchResults.Last.AddNewPos(iLine, iColumn, sLine);

         // Search for functions
         if FindMethod('function '+ ClassNamePrefix, MethodToFind)      // T or C
         and FindImplementor
         then SearchResults.Last.AddNewPos(iLine, iColumn, sLine);
       end;
    end;

  Finalize; // Increment counters
end;



class function TAgent_FindInterface.CanReplace: Boolean;
begin
  Result:= FALSE;
end;





{ FORM }
procedure TAgent_FindInterface.DockSettingsForm(HostPanel: TPanel);
begin
  FormSettings.Container.Parent:= HostPanel;
end;


destructor TAgent_FindInterface.Destroy;
begin
  FreeAndNil(FormSettings);
  inherited;
end;




end.
