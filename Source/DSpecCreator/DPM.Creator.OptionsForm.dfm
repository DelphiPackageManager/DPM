object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  Caption = 'OptionsForm'
  ClientHeight = 504
  ClientWidth = 859
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  inline DPMOptionsFrame: TDPMOptionsFrame
    Left = 0
    Top = 0
    Width = 859
    Height = 463
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 859
    ExplicitHeight = 463
    inherited pgOptions: TPageControl
      Width = 859
      Height = 463
      ExplicitWidth = 859
      ExplicitHeight = 463
      inherited tsSources: TTabSheet
        ExplicitWidth = 851
        ExplicitHeight = 429
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
        end
        inherited Panel2: TPanel
          Top = 218
          Width = 851
          ExplicitTop = 218
          ExplicitWidth = 851
        end
        inherited Panel3: TPanel
          Width = 851
          Height = 129
          ExplicitWidth = 851
          ExplicitHeight = 129
          inherited lvSources: TListView
            Width = 851
            Height = 129
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
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 463
    Width = 859
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitLeft = 688
    ExplicitTop = 432
    ExplicitWidth = 185
    object btnCancel: TButton
      Left = 656
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnOk: TButton
      Left = 764
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      TabOrder = 1
      OnClick = btnOkClick
    end
  end
end
