object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object btnBlockedOp: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Blocked op (in place)'
    TabOrder = 0
    OnClick = btnBlockedOpClick
  end
  object btnBlockedPopup: TButton
    Left = 159
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Blocked op (popup)'
    TabOrder = 1
    OnClick = btnBlockedPopupClick
  end
  object btnLoadStyle: TButton
    Left = 310
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Load VCL style...'
    TabOrder = 2
    OnClick = btnLoadStyleClick
  end
  object btnPopupNoRedraw: TButton
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Popup, no redraw'
    TabOrder = 3
    OnClick = btnPopupNoRedrawClick
  end
  object btnPopupNoLogging: TButton
    Left = 159
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Popup, no logging'
    TabOrder = 4
    OnClick = btnPopupNoLoggingClick
  end
  object btnPartialInvalidate: TButton
    Left = 310
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Partial invalidate'
    TabOrder = 5
    OnClick = btnPartialInvalidateClick
  end
  object btnReshowCycles: TButton
    Left = 461
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Re-show cycles'
    TabOrder = 6
    OnClick = btnReshowCyclesClick
  end
  object dlgOpenStyle: TOpenDialog
    Filter = 'VCL Styles (*.vsf)|*.vsf'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 552
    Top = 16
  end
end
