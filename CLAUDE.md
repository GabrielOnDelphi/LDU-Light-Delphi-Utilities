# CLAUDE.md

## Program Description

### Core Purpose
LDU (LightSaber Delphi Utilities) — VCL toolset that scans/repairs Delphi PAS/DFM source files. Targets legacy code modernization (32→64-bit migration), code-quality fixes, and IDE-related text utilities. Batch-applies all tools recursively across folders.

### Key Features
- 64-bit migration scans: `Pointer(Integer)`/`LongInt` typecasts, `Extended` usage, `SendMessage`/`Perform`/`SetWindowLong` API signatures.
- Code-quality fixes: missing `try/except`, broken `SetFocus`/`CanFocus`, `.Free` → `FreeAndNil`.
- Text/encoding: BOM add/strip, ANSI↔UTF-8 conversion, CRLF line-ending repair (external `FixEnters.exe`), code formatter.
- Search: find all classes implementing an interface; arbitrary code search.
- Misc tools: color picker, options dialog, agent results viewer, optional OTA IDE integration.

## Build System

**Main project**: `LDU.dpr` (project settings in `LDU.dproj`). Project group: `Group.groupproj`.

After BIG changes, run `Build.cmd` to compile via MSBuild.
Also check for FastMM leak report files. If a leak file is found, rename it after you fix the leak.
You will only build in Debug mode.

FastMM4 enabled in DPR (first uses clause). `FastMM_FullDebugMode.dll` shipped in project root.

## Architecture Overview

### Application Flow
- DPR → `TAppData.Create('LUD - Light Delphi Utilities')` → `CreateMainForm(TfrmMain, ...)` → `AppData.Run`.
- `frmMain` (MainForm.pas) — TLightForm with `TCategoryPanelGroup` of buttons. Each button's `Tag` = AgentID.
- Click → `StartTask` → `CreateAgentForm(LastAgent)` (in FormAgent unit) → factory builds an agent and shows results.
- `frmAgentResults` (FormAgent.pas) — runs agent over chosen folder, displays `TSearchResults`.
- `frmEditor` (FormEditor.pas) — in-app PAS viewer/editor for inspecting offending code.
- `frmOptions` (FormOptions.pas) — settings panel; `frmExclude` — folder/file exclusion list.
- `frmOTA` (FormOTA.pas) — Open Tools API integration; sends a file from LDU into the running Delphi IDE.
- `frmClrPick` (FormColorPicker.pas) — standalone modal color picker.
- Per-agent settings forms dock into the agent panel via `TBaseAgent.DockSettingsForm(Panel)`:
  `frmSettingsFindCode`, `frmSettingsIntf`, `frmSettings` (FixLineEndings).

### Key Classes
- `TBaseAgent` (dutBase.pas) — abstract base. Holds `TextBody: TStringList`, `SearchResults: TSearchResults`, `Needle`, `LastPath`, `CaseSensitive`, `Replace`, `FBackupFile`. Loads/saves `LastPath` to INI in ctor/dtor. `Execute(FileName)` → derived class scans; `Finalize` → `DoSave` (writes back if `Replace` & `FFound`, optional `.bak` backup), updates counters. Capabilities: `CanRelax`/`CanReplace` class functions.
- `TDutAgentFactory` (dutAgentFactory.pas) — `CreateAgent(AgentClass, BackupFile)` and `GetAgentDescription(AgentID)`. `IDToClassName(ID)` maps numeric tag → `TAgentClass`. ID ranges: 1–4 fixers, 10–11 find, 20–23 BOM/format, 50–52 WinAPI, 60–61 pointer, 70–71 Extended.
- Concrete agents (each in `dut*.pas`):
  - Upgrade: `TAgent_TryExcept`, `TAgent_SetFocus`, `TAgent_FreeAndNil` (dutUpgradeCode.pas), `TAgent_FixLineEndings` (dutFixLineEndings.pas).
  - Find: `TAgent_FindInterface`, `TAgent_FindCode`.
  - Encoding: `TAgent_BOM_AnsiToUtf`, `TAgent_BOM_Utf2Ansi`, `TAgent_BomExists` (dutBom.pas), `TAgent_CodeFormat` (dutCodeFormat.pas).
  - Win64: `TAgent_APISendMessage`/`APIPerform`/`APISetWindowLong` (dutWin64Api.pas), `TAgent_PointerTypecast`/`PointerLongInt` (dutWin64Pointer.pas), `TAgent_Extended`/`ExtendedPacked` (dutWin64Extended.pas).
- `TfrmMain` (MainForm.pas) — TLightForm; `FormPostInitialize` walks Category panels to restore last-used agent button (via Tag), optionally re-opens it. Persists `LastAgent` in `FormPreRelease`.

### External Modules
- LightSaber framework at `..\..\LightSaber\` (`LightCore.AppData`, `LightCore.SearchResult`, `LightCore.TextFile`, `LightCore.INIFile`, `LightCore.IO`, `LightVcl.Visual.AppData[Form]`, `LightVcl.Common.VclUtils`, `LightVcl.Common.ExecuteShell`).
- Subfolders `_Modules - Integrated`, `_Modules - To be integrated`, `IDE Expert`, plus standalone tools `Tool - Add paths to Library Paths`, `Tool - Read PE_Flag from exe file`, `Tool - TextReplace`.

#CLAUDE INSTRUCTIONS#
Don't give me a summary of this file after you read it.
