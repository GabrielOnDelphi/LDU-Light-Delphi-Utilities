object frmExclude: TfrmExclude
  Left = 0
  Top = 0
  Caption = 'Exclude folders'
  ClientHeight = 229
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 371
    Height = 17
    Hint = 
      'Exclude the following folders from the search.'#13#10'One folder per l' +
      'ine.'
    Align = alTop
    Caption = 'Exclude the following folders from the search:'
  end
  object mmoExclude: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 23
    Width = 371
    Height = 162
    Hint = 
      'Exclude the following folders from the search.'#13#10'One folder per l' +
      'ine.'
    Align = alClient
    Lines.Strings = (
      'c:\MySourceCode\3rd party libs\')
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 188
    Width = 377
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      377
      41)
    object btnApply: TButton
      AlignWithMargins = True
      Left = 164
      Top = 3
      Width = 102
      Height = 35
      Hint = 'Apply'
      Align = alRight
      Caption = 'Apply'
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 272
      Top = 3
      Width = 102
      Height = 35
      Hint = 'Apply and close'
      Align = alRight
      Caption = 'Ok'
      TabOrder = 1
      OnClick = Button1Click
    end
    object chkGlobal: TCheckBox
      Left = 10
      Top = 10
      Width = 97
      Height = 17
      Hint = 'Apply this path to all agents'
      Anchors = [akLeft]
      Caption = 'Global path'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 2
      OnClick = chkGlobalClick
    end
  end
end
