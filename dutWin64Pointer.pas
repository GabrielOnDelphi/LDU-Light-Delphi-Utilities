UNIT dutWin64Pointer;

{=============================================================================================================
   Gabriel Moraru
   2022
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Porting code to 64 bit.
   Checking for 32 bit pointer casts.
-------------------------------------------------------------------------------------------------------------}

//ToDo: check for $FF000000 in most cases, on 64 bit it should be NativeUInt($FF000000).

INTERFACE

USES
  System.SysUtils, System.Classes, LightCore.SearchResult, dutBase;

TYPE
  // Pointer casts
  TAgent_PointerTypecast = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanReplace: Boolean; override;
    class function AgentName: string; override;
  end;

  TAgent_PointerLongInt = class(TBaseAgent)
  public
    procedure Execute(const FileName: string); override;
    class function Description: string; override;
    class function CanRelax: Boolean; override;
    class function AgentName: string; override;
  end;


IMPLEMENTATION

USES
   LightCore.Pascal, LightCore, LightCore.Time;



{=============================================================================================================
   Classic typecast issues
=============================================================================================================}

class function TAgent_PointerTypecast.Description: string;
begin
  Result:= 'On Win64 we cannot assume anymore that SizeOf(Pointer)=SizeOf(Integer/Cardinal/Longint).'+ CRLF+
           'This agent searches for invalid pointer typecasts such as Pointer(Integer,  Pointer(PInteger,  Pointer(Cardinal,  etc, and recomends fixes.'+ CRLF+
           'The Relaxed version searches for the generic "Pointer(", which can result in LOTS of fake positive results!';
end;


procedure TAgent_PointerTypecast.Execute(const FileName: string);
var
   sLine: string;
   iLine: Integer;

   procedure Find(var aLine: String; const Offender, Fix: string);
   begin
       var iColumn:= PosInsensitive(Offender, aLine);
       if iColumn < 1 then Exit;

       FFound:= True;
       SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Win64 incompatible typecast: '+ Offender, ' Use '+ Fix+ ' instead.');

       if Replace then
        begin
         aLine:= ReplaceString(aLine, Offender, Fix);  // This search is insensitive
         TextBody[iLine]:= aLine;
        end;
   end;

begin
  inherited Execute(FileName);

  if NOT FRelaxed then
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];
        if LineIsAComment(sLine) then Continue;

        Find(sLine, 'Pointer(PInteger', 'Pointer(PNativeInt');
        Find(sLine, 'Pointer(Integer' , 'Pointer(NativeInt or PByte( for pointer arithmetics.');
        Find(sLine, 'Pointer(Cardinal', 'Pointer(NativeUInt or PByte( for pointer arithmetics.');
        //Don't search for Pointer(LongInt here. We have a dedicatedfunction for it!
      end
  else
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];
        if LineIsAComment(sLine) then Continue;

        // Search ANY pointer typecasts
        var iColumn:= PosInsensitive('Pointer(', sLine);
        if iColumn > 0 then

          // But ignore valid typecasts
		  todo: this should be Pointer(UIntPtr)
          if  (PosInsensitive('Pointer(NativeInt',  sLine) < 1)
          and (PosInsensitive('Pointer(NativeUInt', sLine) < 1)
          then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Chance of invalid pointer typecast: Pointer(', 'Check if the cast is correct.');
      end;

  Finalize; // Increment counters
end;


class function TAgent_PointerTypecast.CanReplace: Boolean;
begin
  Result:= TRUE;
end;


{ToDo: implement this check:
    if Pos('Move(') > 0 and '*4' or (SizeOf not found) then Found:= true;

    Explanation:
       Correct:
       Move(FSelection[Index + 1], FSelection[Index], (FSelectionCount - Index - 1) * SizeOf(Pointer));

       Wrong:
       Move(FSelection[Index + 1], FSelection[Index], (FSelectionCount - Index - 1) * 4); }












{=============================================================================================================
   LongInt typecasts
=============================================================================================================}

class function TAgent_PointerLongInt.Description: string;
begin
  Result:= 'On Windows, LongInt is always 32bit. We cannot use LongInt in pointer typecasts anymore!'+ CRLF+
           'This agent find invalid LongInt/PLongInt typecasts.';
end;

procedure TAgent_PointerLongInt.Execute(const FileName: string);
var
   sLine: string;
   iLine: Integer;
   iColumn: Integer;
begin
  inherited Execute(FileName);

  if FRelaxed then
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];

        // Search PLongInt first...
        iColumn:= WordPos('PLongInt(', sLine);
        if iColumn > 0
        then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible issue with PLongInt cast.', 'Replace with NPativeInt.')
        else
          begin
            // ...and only after that for LongInt, in order to show the correct msg.
            iColumn:= WordPos('LongInt(', sLine);
            if iColumn > 0
            then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Possible issue with LongInt cast.', 'Replace with NativeInt or PByte (in case of pointer arithmetics).');
          end;
      end
  else
    for iLine:= 0 to TextBody.Count-1 do
      begin
        sLine:= TextBody[iLine];

        { Explicit search }
        iColumn:= WordPos('LongInt(Self)', sLine);
        if iColumn > 0
        then SearchResults.Last.AddNewPos(iLine, iColumn, sLine, 'Issue with LongInt cast.', 'Replace the LongInt with NativeInt.');
      end;

  Finalize; // Increment counters
end;


class function TAgent_PointerLongInt.CanRelax: Boolean;
begin
  Result:= TRUE;
end;


class function TAgent_PointerTypecast.AgentName: string;
begin
  Result:= 'Invalid pointer typecasts';
end;

class function TAgent_PointerLongInt.AgentName: string;
begin
  Result:= 'LongInt typecasts';
end;

end.
