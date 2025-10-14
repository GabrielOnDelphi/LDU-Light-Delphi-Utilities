UNIT dutBase;

{=============================================================================================================
   Gabriel Moraru
   2021
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Base class.
   Provides support for checking PAS files for specific lines of code.
   It holds the list of "Search results".

   The actual search code is implemented in: dutWin64, dutUpgrade, dutUtils which all inherit from TBaseAgent
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.ExtCtrls,
  LightCore.SearchResult, LightCore.TextFile, LightCore.INIFile;

TYPE
  TBaseAgent= class(TObject)
   private
    FBackupFile: Boolean;          // If true, create a backup file IF the file is changed (in case of 'replace')
    procedure DoSave;
    procedure NewFile(const FileName: String);
    procedure LoadSettings;
    procedure SaveSettings;
   protected
    FFound  : Boolean;             // I already have:  SearchResults.Last.Found !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   // The searched text was found
    FRelaxed: Boolean;             // Uses a relaxed search. More exhaustive but can give more false positives
    TextBody: TStringList;
   public
    Needle: string;                // Text to find. Some agents will not use this field.
    LastPath: string;

    // Settings
    Preamble: TWritePreamble;      //Todo: implement this (global GUI)
    CaseSensitive: Boolean;        // Can we convert the Needle/Haystack to lowercase?
    Replace: Boolean;              // The found text should be replaced

    // Results
    FoundFiles: Integer;
    FoundLines: Integer;
    SearchResults: TSearchResults; // A list of results where the issue (invalid code) was found

    constructor Create(aBackupFile: Boolean); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Execute(const FileName: string); virtual;
    procedure Finalize; virtual;
    procedure DockSettingsForm(Panel: TPanel); virtual;

    class function AgentName: string; virtual; abstract;
    class function Description: string; virtual; abstract;

    // Capabilities
    class function CanRelax  : Boolean; virtual;   // True if the agent can do a relaxed search
    class function CanReplace: Boolean; virtual;   // True if the agent can automatically replace the text
  end;


IMPLEMENTATION
USES LightCore.AppData, LightCore.IO;


{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TBaseAgent.Create(aBackupFile: Boolean);
begin
  inherited Create;
  Replace      := FALSE;
  FBackupFile  := aBackupFile;
  TextBody     := TStringList.Create;
  TextBody.Text:= 'Nothing loaded yet!';
  SearchResults:= TSearchResults.Create(True);
  LoadSettings;
end;


destructor TBaseAgent.Destroy;
begin
  SaveSettings;
  FreeAndNil(TextBody);
  FreeAndNil(SearchResults);
  inherited Destroy;
end;


procedure TBaseAgent.Clear;
begin
  FoundFiles:= 0;
  FoundLines:= 0;
  SearchResults.Clear;
end;




{-------------------------------------------------------------------------------------------------------------
   START
-------------------------------------------------------------------------------------------------------------}

{ Derived class must override this method }
procedure TBaseAgent.Execute(const FileName: string);
begin
  NewFile(FileName);
end;


{ Computes statistics and saves results to disk }
procedure TBaseAgent.Finalize;
begin
  DoSave;

  if SearchResults.Last.Found then
    begin
      Inc(FoundFiles);
      Inc(FoundLines, SearchResults.Last.Count);
    end;

  TextBody.Clear;
end;


{ Mark this file as the current file that we work on,
  and add it to the Search Results }
procedure TBaseAgent.NewFile(const FileName: String);
begin
  SearchResults.Add(TSearchResult.Create(FileName));
  TextBody.Text:= StringFromFile(FileName);
end;





{-------------------------------------------------------------------------------------------------------------
   -
-------------------------------------------------------------------------------------------------------------}
procedure TBaseAgent.DoSave;
begin
  if FFound AND Replace then
    begin
      // Do backup
      if FBackupFile
      then BackupFileBak(SearchResults.Last.FileName);

      // Write the new PAS file to disk
      StringToFile(SearchResults.Last.FileName, TextBody.Text, woOverwrite, wpAuto);
    end;
end;


class function TBaseAgent.CanRelax: Boolean;
begin
  Result:= FALSE; // Most agents can't
end;


class function TBaseAgent.CanReplace: Boolean;
begin
  Result:= FALSE; // Most agents can't
end;


procedure TBaseAgent.DockSettingsForm(Panel: TPanel);
begin
  //To be overwritten by child
end;


procedure TBaseAgent.LoadSettings;
var Ini: TIniFileEx;
begin
  if not FileExists(AppDataCore.IniFile) then Exit;
  Ini:= TIniFileEx.Create('AGENTS', AppDataCore.IniFile);
  try
    LastPath:= Ini.Read('LastPath', 'C:\Projects\Delphi');
  finally
    Ini.Free;
  end;
end;


procedure TBaseAgent.SaveSettings;
var Ini: TIniFileEx;
begin
  if not FileExists(AppDataCore.IniFile) then Exit;
  Ini:= TIniFileEx.Create('AGENTS', AppDataCore.IniFile);
  try
    Ini.Write('LastPath', LastPath);
  finally
    Ini.Free;
  end;
end;

end.






