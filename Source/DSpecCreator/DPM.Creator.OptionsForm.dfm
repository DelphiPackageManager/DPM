object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Options'
  ClientHeight = 504
  ClientWidth = 859
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  inline DPMOptionsFrame: TDPMOptionsFrame
    Left = 0
    Top = 0
    Width = 859
    Height = 464
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 859
    ExplicitHeight = 463
    inherited pgOptions: TPageControl
      Width = 859
      Height = 464
      ExplicitWidth = 859
      ExplicitHeight = 463
      inherited tsSources: TTabSheet
        ExplicitWidth = 851
        ExplicitHeight = 430
        inherited Panel1: TPanel
          Width = 851
          ExplicitWidth = 851
          inherited btnAdd: TSpeedButton
            Left = 668
            ExplicitLeft = 668
          end
          inherited btnRemove: TSpeedButton
            Left = 715
            ExplicitLeft = 715
          end
          inherited btnUp: TSpeedButton
            Left = 760
            ExplicitLeft = 760
          end
          inherited btnDown: TSpeedButton
            Left = 805
            ExplicitLeft = 805
          end
          inherited txtPackageCacheLocation: TButtonedEdit
            Height = 23
            ExplicitHeight = 23
          end
        end
        inherited Panel2: TPanel
          Top = 219
          Width = 851
          ExplicitTop = 218
          ExplicitWidth = 851
          inherited txtName: TEdit
            Height = 23
            ExplicitHeight = 23
          end
          inherited txtUri: TButtonedEdit
            Height = 23
            ExplicitHeight = 23
          end
          inherited cboSourceType: TComboBox
            ExplicitHeight = 23
          end
        end
        inherited Panel3: TPanel
          Width = 851
          Height = 130
          ExplicitWidth = 851
          ExplicitHeight = 129
          inherited lvSources: TListView
            Width = 851
            Height = 130
            ExplicitWidth = 851
            ExplicitHeight = 129
          end
        end
      end
      inherited tsIDEOptions: TTabSheet
        inherited pnlIDEOptions: TPanel
          inherited spAutoCloseDelay: TSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
          inherited cboLogLevel: TComboBox
            ExplicitHeight = 23
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 464
    Width = 859
    Height = 40
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 465
    DesignSize = (
      859
      40)
    object btnCancel: TButton
      Left = 764
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnOk: TButton
      Left = 660
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      Default = True
      TabOrder = 1
      OnClick = btnOkClick
    end
  end
end
