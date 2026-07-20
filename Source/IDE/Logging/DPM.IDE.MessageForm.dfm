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
  OldCreateOrder = True
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 376
    Width = 879
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 200
    ExplicitWidth = 863
    DesignSize = (
      879
      49)
    object lblClosing: TLabel
      Left = 144
      Top = 18
      Width = 46
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Closing...'
      Visible = False
      ExplicitTop = 394
    end
    object btnCancel: TButton
      Left = 774
      Top = 12
      Width = 89
      Height = 28
      Action = actCanCancel
      Anchors = [akRight, akBottom]
      TabOrder = 0
      ExplicitLeft = 758
    end
    object btnClose: TButton
      Left = 668
      Top = 12
      Width = 89
      Height = 28
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCloseClick
      ExplicitLeft = 652
    end
    object btnCopy: TButton
      Left = 8
      Top = 12
      Width = 81
      Height = 28
      Action = actCopyLog
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object lblDontClose: TLinkLabel
      Left = 216
      Top = 18
      Width = 95
      Height = 19
      Anchors = [akLeft, akBottom]
      Caption = '[ <a>Cancel closing</a> ]'
      TabOrder = 3
      UseVisualStyle = True
      Visible = False
      OnLinkClick = lblDontCloseLinkClick
    end
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
