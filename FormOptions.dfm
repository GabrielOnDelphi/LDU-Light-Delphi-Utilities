object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 293
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    439
    293)
  TextHeight = 15
  object chkShowAllFiles: TCheckBox
    Left = 24
    Top = 43
    Width = 109
    Height = 17
    Hint = 
      'If checked, lists all scanned files, even those that do not cont' +
      'ained the searched text.'
    Anchors = [akTop, akRight]
    Caption = 'Show all files'
    TabOrder = 0
  end
  object chkBackup: TCheckBox
    Left = 24
    Top = 73
    Width = 214
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Create backup files on replace'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object chkNewWnd: TCheckBox
    Left = 24
    Top = 103
    Width = 214
    Height = 17
    Hint = 'Show the results in a new window or docked.'
    Anchors = [akTop, akRight]
    Caption = 'Show results in a new window'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkSaveStats: TCheckBox
    Left = 25
    Top = 132
    Width = 132
    Height = 17
    Hint = 'Save statistics to disk as "Tool Output.txt"'
    Anchors = [akRight]
    Caption = 'Save statistics'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
