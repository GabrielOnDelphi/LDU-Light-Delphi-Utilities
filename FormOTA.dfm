object frmOTA: TfrmOTA
  Left = 0
  Top = 0
  Caption = 'OTA'
  ClientHeight = 178
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  TextHeight = 15
  object mmoOTA: TMemo
    Left = 21
    Top = 17
    Width = 603
    Height = 85
    Lines.Strings = (
      
        'Filename=c:\My projects\Packages\CubicCommonControls\ccRegistry.' +
        'pas'
      'Line=8'
      'Col=12'
      'Comment=')
    TabOrder = 0
    WordWrap = False
  end
  object btnTest: TButton
    Left = 23
    Top = 115
    Width = 112
    Height = 25
    Caption = 'Send to IDE'
    TabOrder = 1
    OnClick = btnTestClick
  end
end
