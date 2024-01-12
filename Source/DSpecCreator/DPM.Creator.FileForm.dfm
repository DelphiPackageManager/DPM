object SourceForm: TSourceForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'SourceForm'
  ClientHeight = 178
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    464
    178)
  TextHeight = 15
  object lblSrc: TLabel
    Left = 48
    Top = 48
    Width = 19
    Height = 15
    Caption = 'Src:'
  end
  object lblDest: TLabel
    Left = 41
    Top = 100
    Width = 26
    Height = 15
    Caption = 'Dest:'
  end
  object btnCancel: TButton
    Left = 287
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 368
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object edtSource: TEdit
    Left = 74
    Top = 45
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = 'edtSource'
  end
  object chkFlatten: TCheckBox
    Left = 74
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Flatten'
    TabOrder = 3
  end
  object edtDest: TEdit
    Left = 73
    Top = 97
    Width = 370
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Text = 'Edit1'
  end
end
