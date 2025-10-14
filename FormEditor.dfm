object frmEditor: TfrmEditor
  Left = 0
  Top = 0
  Caption = 'File Editor'
  ClientHeight = 462
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnDestroy = FormDestroy
  TextHeight = 15
  object Container: TPanel
    Left = 0
    Top = 0
    Width = 780
    Height = 462
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblCurFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 774
      Height = 15
      Align = alTop
      Caption = 'Current file:'
    end
    object lblDetails: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 21
      Width = 774
      Height = 17
      Align = alTop
      Caption = 'Details:'
      Color = 12375541
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
      WordWrap = True
    end
    object mmoView: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 41
      Width = 774
      Height = 379
      Align = alClient
      Color = clBlack
      Font.Charset = ANSI_CHARSET
      Font.Color = clLime
      Font.Height = -15
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = mmoViewChange
      OnClick = mmoViewClick
      OnMouseDown = mmoViewMouseDown
    end
    object pnlBtm: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 426
      Width = 774
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lblRewind: TLabel
        AlignWithMargins = True
        Left = 262
        Top = 9
        Width = 100
        Height = 15
        Margins.Top = 9
        Margins.Bottom = 9
        Align = alLeft
        Alignment = taCenter
        AutoSize = False
        BiDiMode = bdLeftToRight
        Caption = 'Rewinded'
        Color = 8454143
        ParentBiDiMode = False
        ParentColor = False
        Transparent = False
        Layout = tlCenter
        Visible = False
      end
      object btnSave: TButton
        AlignWithMargins = True
        Left = 680
        Top = 3
        Width = 91
        Height = 27
        Action = actSave
        Align = alRight
        TabOrder = 0
        Visible = False
      end
      object btnOpenIDE: TButton
        AlignWithMargins = True
        Left = 583
        Top = 3
        Width = 91
        Height = 27
        Action = actLoadIDE
        Align = alRight
        Enabled = False
        TabOrder = 1
      end
      object btnPrev: TButton
        AlignWithMargins = True
        Left = 3
        Top = 4
        Width = 81
        Height = 25
        Margins.Top = 4
        Margins.Bottom = 4
        Action = actPrev
        Align = alLeft
        TabOrder = 2
      end
      object btnNext: TButton
        AlignWithMargins = True
        Left = 90
        Top = 4
        Width = 81
        Height = 25
        Margins.Top = 4
        Margins.Bottom = 4
        Action = actNext
        Align = alLeft
        TabOrder = 3
      end
      object btnResetSearch: TButton
        AlignWithMargins = True
        Left = 179
        Top = 5
        Width = 75
        Height = 23
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Action = actResetSearch
        Align = alLeft
        TabOrder = 4
      end
    end
  end
  object TimerRew: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerRewTimer
    Left = 633
    Top = 3
  end
  object ActionList: TActionList
    Left = 540
    Top = 5
    object actNext: TAction
      Caption = 'Next >'
      Hint = 
        'Put the cursor to the next occurence/issue.'#13#10'(The file will be o' +
        'pen into the IDE at this position)'#13#10
      ShortCut = 119
      OnExecute = actNextExecute
    end
    object actPrev: TAction
      Caption = '< Previous'
      Hint = 'Show previous occurence/issue'#13#10
      ShortCut = 118
      OnExecute = actPrevExecute
    end
    object actLoadIDE: TAction
      Caption = 'Open in IDE'
      Hint = 
        'Open this file in IDE, at the CURRENT position (set by the Prev/' +
        'Next buttons).'#13#10'You must have the plugin (BPL) installed in Delp' +
        'hi in order for this function to work!'#13#10
      ShortCut = 120
      OnExecute = actLoadIDEExecute
    end
    object actSave: TAction
      Caption = 'Save'
      Hint = 'Save changes in this file.'
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actResetSearch: TAction
      Caption = 'Reset'
      Hint = 'Reset search in this file.'#13#10
      ShortCut = 116
      OnExecute = actResetSearchExecute
    end
  end
end
