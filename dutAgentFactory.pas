UNIT dutAgentFactory;

{=============================================================================================================
   Gabriel Moraru
   2025
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   xxxxxxx
-------------------------------------------------------------------------------------------------------------}


INTERFACE

USES
  dutBase, System.SysUtils;

TYPE
  TAgentClass = class of TBaseAgent;

  TDutAgentFactory = class
  private
  public
    class function GetAgentDescription(AgentID: Integer): string; static;
    class function CreateAgent(const AgentClass: TAgentClass; BackupFile: Boolean): TBaseAgent;
  end;

function IDToClassName(const ID: Integer): TAgentClass;

IMPLEMENTATION

USES
  dutFixLineEndings, dutWin64Pointer, dutUpgradeCode, dutWin64Api, dutFindInterface, dutBom, dutFindCode, dutWin64Extended, dutCodeFormat;




class function TDutAgentFactory.CreateAgent(const AgentClass: TAgentClass; BackupFile: Boolean): TBaseAgent;
begin
  if NOT Assigned(AgentClass) then
    RAISE Exception.Create('Agent class not assigned');

  Result := TBaseAgent(AgentClass.Create(BackupFile));
end;


class function TDutAgentFactory.GetAgentDescription(AgentID: Integer): string;
begin
  VAR AgentClass:= IDToClassName(AgentID);
  Result := AgentClass.Description;
end;



function IDToClassName(const ID: Integer): TAgentClass;
begin
  case ID of
    // Upgrade code
    1: Result := TAgent_TryExcept;       // Try/Except
    2: Result := TAgent_SetFocus;        // SetFocus
    3: Result := TAgent_FreeAndNil;      // FreeAndNil
    4: Result := TAgent_FixLineEndings;  // ReplaceEnters

    // Find code
    10: Result := TAgent_FindInterface;
    11: Result := TAgent_FindCode;

    // BOM
    20: Result := TAgent_BOM_AnsiToUtf;
    21: Result := TAgent_BOM_Utf2Ansi;
    22: Result := TAgent_BomExists;
    23: Result := TAgent_CodeFormat;
    // -1: Result := TAgent_FixCRLF; //it is an external exe now!

    // WinAPI
    50: Result := TAgent_APISendMessage;
    51: Result := TAgent_APIPerform;
    52: Result := TAgent_APISetWindowLong;

    // Pointer(Integer & Pointer(LongInt
    60: Result := TAgent_PointerTypecast; { Search for 'Pointer(Integer(' . We cannot assume anymorethat SizeOf(Pointer)=SizeOf(Integer/Cardinal/Longint). }
    61: Result := TAgent_PointerLongInt;  { Search for possible LongInt/PLongInt typecasts. On Windows, LongInt is always 32bit! }

    // Extended
    70: Result := TAgent_Extended;        { Search for occurrences of the Extended type and reports them. }
    71: Result := TAgent_ExtendedPacked;

    // Search
    //20: PasParser.ExtractCode;

   else
      RAISE Exception.Create('Unknown agent ID:'+ IntToStr(ID));
  end;



end;


end.
