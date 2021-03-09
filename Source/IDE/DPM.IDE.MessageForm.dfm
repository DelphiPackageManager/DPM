object DPMMessageForm: TDPMMessageForm
  Left = 0
  Top = 0
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
  DesignSize = (
    781
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 8
    Top = 8
    Width = 764
    Height = 361
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Cancel: TButton
    Left = 672
    Top = 386
    Width = 100
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
  end
end
