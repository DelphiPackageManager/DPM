object DependencyForm: TDependencyForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Dependency'
  ClientHeight = 121
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
    121)
  TextHeight = 15
  object lblBuildId: TLabel
    Left = 44
    Top = 16
    Width = 13
    Height = 15
    Caption = 'Id:'
  end
  object lblProject: TLabel
    Left = 16
    Top = 45
    Width = 41
    Height = 15
    Caption = 'Version:'
  end
  object btnCancel: TButton
    Left = 318
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 399
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOkClick
  end
  object edtDependencyId: TEdit
    Left = 72
    Top = 13
    Width = 394
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TextHint = 'e.g VSoft.DUnitX'
  end
  object edtVersion: TEdit
    Left = 72
    Top = 42
    Width = 394
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'semver - e.g 1.2.0'
  end
end
