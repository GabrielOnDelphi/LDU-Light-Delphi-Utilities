 Code Review — LDU project

  Critical

  1. DoSave reads FFound but Finalize reads SearchResults.Last.Found (dutBase.pas:146 vs :119). Two parallel "found"
  signals. Future agents that only use SearchResults.Last.AddNewPos won't trigger writes in Replace mode. Source itself
  flags it (line 31). Fix: drop FFound, use SearchResults.Last.Found.
  2. TAgent_FixLineEndings shadows base FFound (dutFixLineEndings.pas:36). Local private FFound hides protected base
  field ? DoSave always sees FALSE ? Replace silently never writes. Fix: delete local field.
  3. Hard as TCategoryPanelSurface cast (MainForm.pas:117). Crashes if TCategoryPanel ever holds a non-surface child
  (header/expand button). Fix: is-check then Continue.

  Significant

  4. SaveSettings bails when INI missing (dutBase.pas:191). First-run loses LastPath. Fix: remove the FileExists guard
  from Save (keep on Load).
  5. Use-after-free risk in agent destructors re-parenting FormSettings.Container before FreeAndNil
  (dutFindCode.pas:128, dutFindInterface.pas:82, dutFixLineEndings.pas:137). AppData may have already freed the form.
  Fix: if Assigned(FormSettings) then ... or own the form locally with Create(nil).
  6. Unbounded Positions[] access (FormEditor.pas:183, :193). scrollToPos(0) after empty results crashes; showDetails
  indexes without bounds check.

  Minor

  7. dutCodeFormat.Execute writes new file unconditionally (:117) — ignores Replace flag, creates files even in
  search-only mode.
  8. Bare except swallowing all (FormColorPicker.pas:70) — violates project rule. Narrow to EConvertError.

  Clean

  dutWin64Api.pas, dutWin64Extended.pas, dutWin64Pointer.pas, dutBom.pas, dutAgentFactory.pas, FormExclude.pas,
  FormOTA.pas.

  Top fixes

  #2 (silent breakage) ? #1 (latent hazard) ? #3 (crash) ? #4 (data loss).

? Brewed for 3m 27s