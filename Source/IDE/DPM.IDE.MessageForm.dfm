object DPMMessageForm: TDPMMessageForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'DPM Package Manager'
  ClientHeight = 433
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnHide = FormHide
  DesignSize = (
    781
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object lblClosing: TLabel
    Left = 144
    Top = 397
    Width = 46
    Height = 13
    Caption = 'Closing...'
    Visible = False
  end
  object btnCancel: TButton
    Left = 672
    Top = 386
    Width = 100
    Height = 35
    Action = actCanCancel
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object btnCopy: TButton
    Left = 8
    Top = 386
    Width = 89
    Height = 35
    Action = actCopyLog
    TabOrder = 2
  end
  object btnClose: TButton
    Left = 544
    Top = 386
    Width = 100
    Height = 35
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object lblDontClose: TLinkLabel
    Left = 216
    Top = 396
    Width = 95
    Height = 19
    Caption = '[ <a>Cancel closing</a> ]'
    TabOrder = 3
    UseVisualStyle = True
    Visible = False
    OnLinkClick = lblDontCloseLinkClick
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 312
    Top = 312
    object actCanCancel: TAction
      Caption = 'Cancel'
      ShortCut = 27
      OnExecute = actCanCancelExecute
    end
    object actCopyLog: TAction
      Caption = 'Copy'
      OnExecute = actCopyLogExecute
    end
  end
  object ClosingInTimer: TTimer
    Enabled = False
    OnTimer = ClosingInTimerTimer
    Left = 40
    Top = 312
  end
end
