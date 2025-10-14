object frmMain: TfrmMain
  Left = 286
  Top = 124
  AlphaBlend = True
  AlphaBlendValue = 250
  Caption = 'LDU (Light Delphi Utilities) - Gabriel Moraru 2022'
  ClientHeight = 567
  ClientWidth = 769
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  DesignSize = (
    769
    567)
  TextHeight = 17
  object lblDescription: TLabel
    AlignWithMargins = True
    Left = 196
    Top = 135
    Width = 568
    Height = 188
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Agent description'
    WordWrap = True
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 193
    Height = 567
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Categories: TCategoryPanelGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 187
      Height = 515
      VertScrollBar.Tracking = True
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -12
      HeaderFont.Name = 'Segoe UI'
      HeaderFont.Style = []
      ParentBackground = True
      ParentColor = True
      TabOrder = 0
      object catTools: TCategoryPanel
        Top = 391
        Height = 91
        Caption = 'Other tools'
        TabOrder = 0
        object btnColorPick: TButton
          Tag = 5
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Color picker'
          TabOrder = 0
          OnClick = btnColorPickClick
        end
        object Button2: TButton
          Tag = 23
          AlignWithMargins = True
          Left = 3
          Top = 2
          Width = 175
          Height = 26
          Hint = '----------'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Format source code'
          TabOrder = 1
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
      end
      object catImprove: TCategoryPanel
        Top = 271
        Height = 120
        Caption = 'Improve code'
        TabOrder = 1
        object btnFreeAndNil3: TButton
          Tag = 3
          AlignWithMargins = True
          Left = 3
          Top = 62
          Width = 175
          Height = 26
          Hint = 'Replaces Object.Free with FreeAndNil(Object) which is safer.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = '.Free -> FreeAndNil()'
          TabOrder = 0
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnTryExcept4: TButton
          Tag = 1
          AlignWithMargins = True
          Left = 3
          Top = 2
          Width = 175
          Height = 26
          Hint = 'Finds all try/except lines. See details.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Try/Except'
          TabOrder = 1
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object setFocus4: TButton
          Tag = 2
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 175
          Height = 26
          Hint = 
            'SetFocus is broken in Delphi. Replaces MyControl.SetFocus with a' +
            ' my better alternative.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Tcontrol.SetFocus'
          TabOrder = 2
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
      end
      object cat64bit: TCategoryPanel
        Top = 241
        Height = 30
        Caption = '32 to 64-bit upgrade'
        Collapsed = True
        TabOrder = 2
        ExpandedHeight = 240
        object btnAgExtended: TButton
          Tag = 70
          AlignWithMargins = True
          Left = 3
          Top = 152
          Width = 175
          Height = 26
          Hint = 'It is recommended to replace Extended with Double.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Extended'
          TabOrder = 0
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgExtendedRec: TButton
          Tag = 71
          AlignWithMargins = True
          Left = 3
          Top = 182
          Width = 175
          Height = 26
          Hint = 
            'Find packed records that have an '#39'Extended'#39' fields.'#13#10'The "packed' +
            '" keyword can indicate that the record might be saved to disk.'#13#10 +
            'In this case we need to make sure that the size of the data rema' +
            'ins the same, no matter if we are on Win32 or Win 64.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Extended in Records'
          TabOrder = 1
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgPointer: TButton
          Tag = 60
          AlignWithMargins = True
          Left = 3
          Top = 92
          Width = 175
          Height = 26
          Hint = 'Searches for "Pointer(Integer(" and similar issues.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Invalid pointer casts'
          TabOrder = 2
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgLongInt: TButton
          Tag = 61
          AlignWithMargins = True
          Left = 3
          Top = 122
          Width = 175
          Height = 26
          Hint = 
            'Find possible LongInt/PLongInt typecasts.'#13#10'On Windows, LongInt i' +
            's always 32bit!'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'LongInt casts'
          TabOrder = 3
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgSendMsgTypeCst: TButton
          Tag = 50
          AlignWithMargins = True
          Left = 3
          Top = 2
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'SendMessage'
          TabOrder = 4
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgPerform: TButton
          Tag = 51
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'TComponent.Perform'
          TabOrder = 5
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgWinLong: TButton
          Tag = 52
          AlignWithMargins = True
          Left = 3
          Top = 62
          Width = 175
          Height = 26
          Hint = 
            'Search SetWindowLong/GetWindowLog .'#13#10#13#10'Replace SetWindowLong/Get' +
            'WindowLog with SetWindowLongPtr/GetWindowLongPtr for GWLP_HINSTA' +
            'NCE, GWLP_ID, GWLP_USERDATA, GWLP_HWNDPARENT and GWLP_WNDPROC as' +
            ' they return pointers and handles. Pointers that are passed to S' +
            'etWindowLongPtr should be type-casted to LONG_PTR and not to Int' +
            'eger/Longint.'#13#10#13#10'    Wrong:'#13#10'        SetWindowLong(hWnd, GWL_WND' +
            'PROC, Longint(@MyWindowProc));'#13#10#13#10'    Correct:'#13#10'        SetWindo' +
            'wLongPtr(hWnd, GWLP_WNDPROC, LONG_PTR(@MyWindowProc));'#13#10
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'SetWindowLong'
          TabOrder = 6
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
      end
      object catText: TCategoryPanel
        Top = 90
        Height = 151
        Caption = 'Text files'
        TabOrder = 3
        object btnBOM: TButton
          Tag = 22
          AlignWithMargins = True
          Left = 3
          Top = 2
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Has BOM?'
          TabOrder = 0
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnBom_Ansi2Utf: TButton
          Tag = 20
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Ansi to UTF'
          TabOrder = 1
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnBom_Utf2Ansi: TButton
          Tag = 21
          AlignWithMargins = True
          Left = 3
          Top = 62
          Width = 175
          Height = 26
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'UTF to Ansi'
          TabOrder = 2
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnCrLf: TButton
          Tag = 4
          AlignWithMargins = True
          Left = 3
          Top = 92
          Width = 175
          Height = 26
          Hint = 
            'Fixes invalid Enter characters, to match the Windows standard (C' +
            'R + LF)'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Fix CRLF'
          TabOrder = 3
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
      end
      object catSearch: TCategoryPanel
        Top = 0
        Height = 90
        Caption = 'Search'
        TabOrder = 4
        object btnAgIntfImpl: TButton
          Tag = 10
          AlignWithMargins = True
          Left = 3
          Top = 2
          Width = 175
          Height = 26
          Hint = 
            'Search in all files for the class that implements the specified ' +
            'interface.'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Interface implementor'
          TabOrder = 0
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
        object btnAgFindCode: TButton
          Tag = 11
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 175
          Height = 26
          Hint = 'List all lines of code that contains the specified item'
          Margins.Top = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Find code'
          TabOrder = 1
          WordWrap = True
          OnClick = StartTask
          OnMouseEnter = btnMouseEnter
        end
      end
    end
    object chkReopenLast: TCubicCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 547
      Width = 187
      Height = 17
      Hint = 'When the program starts, reopen the last used agent'
      Align = alBottom
      Caption = 'Reopen last agent'
      Checked = True
      State = cbChecked
      TabOrder = 1
      AutoSize = True
    end
    object chkHideMainForm: TCubicCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 524
      Width = 187
      Height = 17
      Hint = 'Hide this form when an agent is open'
      Align = alBottom
      Caption = 'Hide main form after agent'
      TabOrder = 2
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 196
    Top = 5
    Width = 568
    Height = 123
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    object btnSettings: TButton
      Tag = 3
      AlignWithMargins = True
      Left = 127
      Top = 47
      Width = 179
      Height = 28
      Caption = 'Global settings'
      TabOrder = 0
      OnClick = btnSettingsClick
    end
    object btnHelp2: TButton
      AlignWithMargins = True
      Left = 322
      Top = 46
      Width = 147
      Height = 29
      Caption = 'SetFocus details'
      TabOrder = 1
      WordWrap = True
      OnClick = btnHelp2Click
    end
  end
end
