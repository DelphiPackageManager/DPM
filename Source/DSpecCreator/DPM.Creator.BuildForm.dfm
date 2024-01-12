object BuildForm: TBuildForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Build'
  ClientHeight = 168
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    522
    168)
  TextHeight = 15
  object lblBuildId: TLabel
    Left = 40
    Top = 48
    Width = 40
    Height = 15
    Caption = 'BuildId:'
  end
  object lblProject: TLabel
    Left = 40
    Top = 77
    Width = 40
    Height = 15
    Caption = 'Project:'
  end
  object btnCancel: TButton
    Left = 351
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 432
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object edtBuildId: TEdit
    Left = 96
    Top = 45
    Width = 411
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'edtBuildId'
  end
  object edtProject: TEdit
    Left = 96
    Top = 74
    Width = 411
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = 'Edit1'
  end
end
