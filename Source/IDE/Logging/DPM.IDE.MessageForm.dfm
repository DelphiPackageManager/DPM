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
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    879
    425)
  TextHeight = 13
  object lblClosing: TLabel
    Left = 144
    Top = 394
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Closing...'
    Visible = False
  end
  object btnCancel: TButton
    Left = 758
    Top = 388
    Width = 89
    Height = 28
    Action = actCanCancel
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object btnCopy: TButton
    Left = 8
    Top = 388
    Width = 81
    Height = 28
    Action = actCopyLog
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object btnClose: TButton
    Left = 652
    Top = 389
    Width = 89
    Height = 28
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object lblDontClose: TLinkLabel
    Left = 216
    Top = 393
    Width = 95
    Height = 19
    Anchors = [akLeft, akBottom]
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
