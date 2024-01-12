object TemplateForm: TTemplateForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Templates'
  ClientHeight = 142
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 32
    Top = 32
    Width = 121
    Height = 15
    Caption = 'Enter Template Names:'
  end
  object edtTemplate: TEdit
    Left = 32
    Top = 56
    Width = 233
    Height = 23
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 109
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 190
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
  end
end
