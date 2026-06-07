object BuildForm: TBuildForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Build'
  ClientHeight = 129
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
    129)
  TextHeight = 15
  object lblProject: TLabel
    Left = 16
    Top = 45
    Width = 80
    Height = 15
    Caption = 'Project (.dproj):'
  end
  object btnCancel: TButton
    Left = 306
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 395
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
  end
  object edtProject: TEdit
    Left = 102
    Top = 42
    Width = 368
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
end
