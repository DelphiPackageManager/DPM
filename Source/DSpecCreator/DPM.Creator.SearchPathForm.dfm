object SearchPathForm: TSearchPathForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'SearchPathForm'
  ClientHeight = 133
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    486
    133)
  TextHeight = 15
  object lblRuntimeSrc: TLabel
    Left = 34
    Top = 32
    Width = 19
    Height = 15
    Caption = 'Src:'
  end
  object btnCancel: TButton
    Left = 316
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 397
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object edtSearchPath: TEdit
    Left = 59
    Top = 29
    Width = 413
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
end
