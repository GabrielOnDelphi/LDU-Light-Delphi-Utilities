UNIT dutTextReplace;

{ Replaces text }

INTERFACE

USES
  System.SysUtils, System.Classes, dutBase;

TYPE
  TDUTReplace= class(TDUTBase)
   private
   public
    constructor Create; override;
    destructor Destroy; override;
    procedure SearchBetween (CONST TagStart, TagEnd, ReplaceWith: string);
    procedure ReplaceBetween(CONST TagStart, TagEnd, ReplaceWith: string; EliminateTags: Boolean);
  end;


IMPLEMENTATION

USES LightCore.SearchResult, LightCore.IO, LightCore.TextFile, LightVcl.Common.IO, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.Dialogs,
     LightVcl.Graph.Util, LightVcl.Common.WinVersion, LightVcl.Common.WinVersionAPI, LightVcl.Common.ExeVersion, LightVcl.Common.CenterControl, LightCore.WrapString,LightVcl.Internet.HTML, LightVcl.Common.Sound, LightVcl.Common.Debugger,
     LightCore.StringList, LightVcl.Internet.Common, LightCore.Internet, LightVcl.Common.Shell, LightVcl.Common.SystemTime, LightVcl.Common.Clipboard,
     LightVcl.Visual.AssociateExt, LightVcl.Common.ExecuteShell, LightCore.AppData, LightVcl.Visual.AppData
;


constructor TDUTReplace.Create;
begin
  inherited Create;
end;


destructor TDUTReplace.Destroy;
begin
  inherited Destroy;
end;


procedure TDUTReplace.ReplaceBetween(CONST TagStart, TagEnd, ReplaceWith: string; EliminateTags: Boolean);
var
  TextBody: string;
  FFound: Boolean;x

  LastPos: Integer;
begin
  LastPos:= 1;
  Found:= FALSE;

  TextBody:= StringFromFile(SearchResults.Last.FileName);
  REPEAT
    TextBody:= LightCore.ReplaceBetween(TextBody, TagStart, TagEnd, ReplaceWith, LastPos, EliminateTags, LastPos);
    if LastPos> 0
    then
      begin
        //TODO: CONVERT LastPos to LineNumber!!!!!!!!!!!!! We could loop through a list of strings to see on which line the <TagStart> is found
        SearchResults.Last.AddNewPos(LastPos, 0, '', 'Text found', 'Text replaced');
        Found:= TRUE;
      end
    else Break;

    LastPos:= LastPos+ Length(TagStart);
  UNTIL LastPos < 1;

  if Found then
    begin
      if BackupFile
      then BackupFileBak(SearchResults.Last.FileName);
      TRY
        if CanWriteToFolder(SearchResults.Last.FileName)
        then StringToFile(SearchResults.Last.FileName, TextBody, woOverwrite, TRUE)
      EXCEPT
        SearchResults.Last.AddNewPos(0, 0, '', 'ERROR! CANNOT WRITE TO DISK!', '');
      END;
    end;
end;


procedure TDUTReplace.SearchBetween(CONST TagStart, TagEnd, ReplaceWith: string);
var
  TextBody: string;
  FFound: Boolean;x

  LastPos: Integer;
begin
  LastPos:= 1;
  TextBody:= StringFromFile(SearchResults.Last.FileName);
  REPEAT
    LastPos:= LightCore.SearchBetween(TextBody, TagStart, TagEnd, LastPos+ Length(TagStart));
    Found:= LastPos> 0;
    if Found
    then SearchResults.Last.AddNewPos(LastPos, 0, '', 'Tags found', '');
  UNTIL NOT Found;
end;


end.
