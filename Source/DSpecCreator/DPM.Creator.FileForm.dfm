object SourceForm: TSourceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'SourceForm'
  ClientHeight = 147
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
    147)
  TextHeight = 15
  object lblSrc: TLabel
    Left = 22
    Top = 16
    Width = 19
    Height = 15
    Caption = 'Src:'
  end
  object lblDest: TLabel
    Left = 14
    Top = 69
    Width = 26
    Height = 15
    Caption = 'Dest:'
  end
  object btnCancel: TButton
    Left = 323
    Top = 114
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 404
    Top = 114
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOkClick
  end
  object edtSource: TEdit
    Left = 46
    Top = 13
    Width = 429
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object chkFlatten: TCheckBox
    Left = 47
    Top = 43
    Width = 97
    Height = 17
    Caption = 'Flatten'
    TabOrder = 1
  end
  object edtDest: TEdit
    Left = 46
    Top = 66
    Width = 429
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
end
