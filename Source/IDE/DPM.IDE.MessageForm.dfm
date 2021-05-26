object DPMMessageForm: TDPMMessageForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'DPM Package Manager'
  ClientHeight = 425
  ClientWidth = 879
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 575
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  DesignSize = (
    879
    425)
  PixelsPerInch = 96
  TextHeight = 13
  object lblClosing: TLabel
    Left = 144
    Top = 389
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Closing...'
    Visible = False
    ExplicitTop = 397
  end
  object btnCancel: TButton
    Left = 770
    Top = 378
    Width = 100
    Height = 35
    Action = actCanCancel
    Anchors = [akRight, akBottom]
    TabOrder = 0
    ExplicitLeft = 672
    ExplicitTop = 386
  end
  object btnCopy: TButton
    Left = 8
    Top = 378
    Width = 89
    Height = 35
    Action = actCopyLog
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    ExplicitTop = 386
  end
  object btnClose: TButton
    Left = 642
    Top = 378
    Width = 100
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
    ExplicitLeft = 544
    ExplicitTop = 386
  end
  object lblDontClose: TLinkLabel
    Left = 216
    Top = 388
    Width = 95
    Height = 19
    Anchors = [akLeft, akBottom]
    Caption = '[ <a>Cancel closing</a> ]'
    TabOrder = 3
    UseVisualStyle = True
    Visible = False
    OnLinkClick = lblDontCloseLinkClick
    ExplicitTop = 396
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
