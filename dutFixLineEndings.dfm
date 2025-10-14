object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = '@Settings'
  ClientHeight = 140
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ShowHint = True
  TextHeight = 15
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 289
    Height = 132
    Align = alTop
    TabOrder = 0
    DesignSize = (
      289
      132)
    object chkNbsp: TCheckBox
      Left = 15
      Top = 57
      Width = 266
      Height = 17
      Hint = 'Replace character #160 (A0) with space'
      Anchors = [akLeft]
      Caption = 'Replace non breaking space character'
      TabOrder = 0
    end
  end
end
