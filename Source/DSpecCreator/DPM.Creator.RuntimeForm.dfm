object BplForm: TBplForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Runtime'
  ClientHeight = 159
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    487
    159)
  TextHeight = 15
  object lblRuntimeSrc: TLabel
    Left = 27
    Top = 56
    Width = 19
    Height = 15
    Caption = 'Src:'
  end
  object lblRuntimeBuildId: TLabel
    Left = 10
    Top = 16
    Width = 43
    Height = 15
    Caption = 'Build Id:'
  end
  object btnCancel: TButton
    Left = 316
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 397
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = btnOkClick
  end
  object chkCopyLocal: TCheckBox
    Left = 59
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Copy Local'
    TabOrder = 2
  end
  object edtSource: TEdit
    Left = 59
    Top = 53
    Width = 413
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object edtBuildId: TEdit
    Left = 59
    Top = 13
    Width = 413
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object chkInstall: TCheckBox
    Left = 59
    Top = 111
    Width = 97
    Height = 17
    Caption = 'Install In IDE'
    TabOrder = 3
  end
end
