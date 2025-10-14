object frmClrPick: TfrmClrPick
  Left = 0
  Top = 0
  Caption = 'Delphi Color Picker'
  ClientHeight = 299
  ClientWidth = 601
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnCreate = FormCreate
  TextHeight = 17
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 595
    Height = 293
    Align = alClient
    Caption = 'Container'
    TabOrder = 0
    object Label4: TLabel
      Left = 1
      Top = 275
      Width = 35
      Height = 17
      Cursor = crHandPoint
      Align = alBottom
      Alignment = taCenter
      Caption = 'Home'
      OnClick = Label4Click
    end
    object lblHTML: TLabel
      Left = 20
      Top = 90
      Width = 45
      Height = 17
      Cursor = crHandPoint
      Hint = 'Click to copy to clipboard'
      Caption = 'Html clr'
      ParentShowHint = False
      ShowHint = True
      OnClick = CopyToClipboard_Click
    end
    object lblDelphiHex: TLabel
      Left = 20
      Top = 63
      Width = 38
      Height = 17
      Cursor = crHandPoint
      Caption = 'Label1'
      OnClick = lblDelphiHexClick
    end
    object Label2: TLabel
      Left = 20
      Top = 37
      Width = 81
      Height = 17
      Caption = 'Delphi clr (int)'
    end
    object Label1: TLabel
      Left = 20
      Top = 11
      Width = 73
      Height = 17
      Caption = 'Delphi name'
    end
    object Panel1: TPanel
      Left = 217
      Top = 1
      Width = 377
      Height = 274
      Align = alRight
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Your color'
      ParentBackground = False
      TabOrder = 0
    end
    object edtEnterClr: TLabeledEdit
      Left = 20
      Top = 150
      Width = 121
      Height = 25
      EditLabel.Width = 146
      EditLabel.Height = 17
      EditLabel.Caption = 'Enter color (format BGR):'
      TabOrder = 1
      Text = '$00FF00'
      OnChange = edtEnterClrChange
      OnKeyDown = edtEnterClrKeyDown
    end
    object btnChooseClr: TButton
      Left = 20
      Top = 182
      Width = 121
      Height = 38
      Caption = 'Choose color'
      TabOrder = 2
      OnClick = btnChooseClrClick
    end
    object btnClipbrd: TButton
      Left = 20
      Top = 226
      Width = 121
      Height = 38
      Caption = 'Copy color'
      TabOrder = 3
      OnClick = CopyToClipboard_Click
    end
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 343
    Top = 69
  end
end
