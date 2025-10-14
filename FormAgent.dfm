object frmAgentResults: TfrmAgentResults
  Left = 0
  Top = 0
  Caption = 'Search results'
  ClientHeight = 508
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  TextHeight = 17
  object splVertical: TSplitter
    Left = 658
    Top = 54
    Height = 454
    Align = alRight
    ResizeStyle = rsUpdate
  end
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 57
    Width = 652
    Height = 448
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object splResults: TSplitter
      AlignWithMargins = True
      Left = 3
      Top = 184
      Width = 646
      Height = 6
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      MinSize = 10
      ResizeStyle = rsUpdate
      Visible = False
    end
    object pnlFiles: TPanel
      Left = 0
      Top = 0
      Width = 652
      Height = 181
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblInpOut: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 646
        Height = 17
        Align = alTop
        Caption = 'Input/Output'
      end
      object lbxResults: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 23
        Width = 646
        Height = 155
        Hint = 
          'Click to list the content of the file.'#13#10'Double click to load fil' +
          'e in the Delphi IDE.'#13#10'Right click for more options.'
        Align = alClient
        ItemHeight = 17
        PopupMenu = PopupMenu
        TabOrder = 0
        OnClick = lbxResultsClick
        OnDblClick = lbxResultsDblClick
      end
    end
  end
  object pnlRight: TPanel
    AlignWithMargins = True
    Left = 664
    Top = 57
    Width = 197
    Height = 448
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnSearch: TButton
      Tag = 9
      AlignWithMargins = True
      Left = 11
      Top = 318
      Width = 175
      Height = 42
      Margins.Left = 11
      Margins.Right = 11
      Align = alBottom
      Caption = 'Search'
      TabOrder = 0
      OnClick = btnSearchClick
    end
    object btnReplace: TButton
      Tag = 9
      AlignWithMargins = True
      Left = 11
      Top = 366
      Width = 175
      Height = 42
      Margins.Left = 11
      Margins.Right = 11
      Align = alBottom
      Caption = 'Replace'
      TabOrder = 1
      OnClick = btnReplaceClick
    end
    object mmoStats: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 191
      Height = 110
      Align = alTop
      Color = clBtnFace
      TabOrder = 2
      Visible = False
    end
    object chkRelaxed: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 295
      Width = 191
      Height = 17
      Hint = 
        'Uses a relaxed search. '#13#10'More exhaustive but can give more false' +
        ' positives'
      Align = alBottom
      Caption = 'Relaxed search'
      TabOrder = 3
    end
    object btnSave: TButton
      Tag = 9
      AlignWithMargins = True
      Left = 11
      Top = 414
      Width = 175
      Height = 31
      Hint = 'Save all lines of code where the searched string was found'
      Margins.Left = 11
      Margins.Right = 11
      Align = alBottom
      Caption = 'Save results'
      Enabled = False
      TabOrder = 4
      OnClick = btnSaveClick
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 272
      Width = 191
      Height = 17
      Action = actShowPanel
      Align = alBottom
      State = cbChecked
      TabOrder = 5
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 858
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      858
      48)
    object edtFilter: TLabeledEdit
      Left = 692
      Top = 19
      Width = 73
      Height = 25
      Hint = 'Multiple extensions can be provided.'#13#10'Example: *.pas;*.dpr;*.inc'
      Anchors = [akTop, akRight]
      EditLabel.Width = 49
      EditLabel.Height = 17
      EditLabel.Caption = 'File filter'
      TabOrder = 0
      Text = '*.pas;*.inc'
    end
    object btnExclude: TButton
      Left = 772
      Top = 16
      Width = 75
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Exclude...'
      TabOrder = 1
      OnClick = btnExcludeClick
    end
    object edtPath: TCubicPathEdit
      Left = 0
      Top = 0
      Width = 683
      Height = 48
      Path = 'C:\Projects\'
      ShowCreateBtn = False
      OnPathChanged = edtPathPathChanged
      Align = alLeft
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Search in this folder'
      TabOrder = 2
    end
  end
  object PopupMenu: TPopupMenu
    Left = 392
    Top = 378
    object mnuCopyName: TMenuItem
      Caption = 'Copy filename'
      OnClick = mnuCopyNameClick
    end
    object mnuOpen: TMenuItem
      Caption = 'Open'
      OnClick = mnuOpenClick
    end
  end
  object DelaySearchFiles: TTimer
    Enabled = False
    Interval = 1500
    OnTimer = DelaySearchFilesTimer
    Left = 392
    Top = 256
  end
  object ActionList: TActionList
    Left = 391
    Top = 316
    object actShowPanel: TAction
      AutoCheck = True
      Caption = 'Show right panel (Ctrl+R)'
      Checked = True
      ShortCut = 16466
      OnExecute = actShowPanelExecute
    end
  end
end
