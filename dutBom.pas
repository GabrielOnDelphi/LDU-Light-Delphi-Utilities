UNIT dutBom;

{=============================================================================================================
   Gabriel Moraru
   2022
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   UTF8
   Convert between ANSI and UTF8

   Wikipedia:
     The UTF-8 representation of the BOM is the (hexadecimal) byte sequence EF BB BF .
     The Unicode Standard permits the BOM in UTF-8, but does not require or recommend its use.
=============================================================================================================}

//ToDo: use function  ForceAddBOM    (CONST FileName: string): Boolean;
//ToDo: use function  ForceRemoveBOM (CONST FileName: string): Boolean;


INTERFACE

USES
  System.SysUtils, LightCore.SearchResult, dutBase;

TYPE
  TAgent_BomExists = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

  TAgent_BOM_AnsiToUtf = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;

  TAgent_BOM_Utf2Ansi = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function AgentName: string; override;
  end;



IMPLEMENTATION

USES
   LightCore.Pascal, LightCore, LightCore.Time, LightCore.Types, LightCore.TextFile;



{=============================================================================================================
   HAS BOM?
=============================================================================================================}

class function TAgent_BomExists.Description: string;
begin
  Result:= 'Shows if the files in the specified folder have BOM or not.'+CRLF+
           'ToDo: Add/remove BOM to the file(s).';
end;


procedure TAgent_BomExists.Execute(const FileName: string);
begin
  inherited Execute(FileName);

  if NOT LightCore.TextFile.FileHasBOM(SearchResults.Last.FileName)
  then SearchResults.Last.AddNewPos('File without BOM.');         // We indicate that this file was found without BOM

  Finalize;
end;







{=============================================================================================================
   Convert Ansi To Utf
=============================================================================================================}
class function TAgent_BOM_AnsiToUtf.Description: string;
begin
  Result:= 'Converts ANSI file to UTF8 (and adds BOM to the file).';
end;


procedure TAgent_BOM_AnsiToUtf.Execute(const FileName: string);
begin
  inherited Execute(FileName);

  if NOT LightCore.TextFile.FileHasBOM(SearchResults.Last.FileName) then
   begin
     LightCore.TextFile.ConvertToUTF(SearchResults.Last.FileName);
     SearchResults.Last.AddNewPos('Converted to UTF8 (with BOM).');
   end;

  Finalize; // Increment counters
end;








{=============================================================================================================
   Convert UTF To Ansi
=============================================================================================================}
class function TAgent_BOM_Utf2Ansi.Description: string;
begin
  Result:= 'Converts UTF8 file to ANSI (and adds BOM to the file).';
end;


procedure TAgent_BOM_Utf2Ansi.Execute(const FileName: string);
begin
  inherited Execute(FileName);

  if LightCore.TextFile.ConvertToAnsi(SearchResults.Last.FileName)
  then SearchResults.Last.AddNewPos('Converted to ANSI');

  Finalize; // Increment counters
end;









class function TAgent_BomExists.AgentName: string;
begin
  Result:= 'Has BOM?';
end;

class function TAgent_BOM_AnsiToUtf.AgentName: string;
begin
  Result:= 'Ansi to Utf';
end;

class function TAgent_BOM_Utf2Ansi.AgentName: string;
begin
  Result:= 'Utf to Ansi';
end;

end.
