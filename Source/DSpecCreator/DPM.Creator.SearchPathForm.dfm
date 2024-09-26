object SearchPathForm: TSearchPathForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Search Path Entry'
  ClientHeight = 103
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
    103)
  TextHeight = 15
  object lblRuntimeSrc: TLabel
    Left = 23
    Top = 16
    Width = 30
    Height = 15
    Caption = 'Path :'
  end
  object btnCancel: TButton
    Left = 316
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 397
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
  end
  object edtSearchPath: TEdit
    Left = 59
    Top = 13
    Width = 410
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
end
