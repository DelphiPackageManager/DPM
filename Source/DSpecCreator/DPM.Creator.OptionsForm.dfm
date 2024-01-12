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
          StyleElements = [seFont, seClient, seBorder]
          ExplicitWidth = 851
          inherited lblPackageSources: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label3: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
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
            StyleElements = [seFont, seClient, seBorder]
          end
        end
        inherited Panel2: TPanel
          Top = 218
          Width = 851
          StyleElements = [seFont, seClient, seBorder]
          ExplicitTop = 218
          ExplicitWidth = 851
          inherited Label1: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label2: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label5: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label6: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label8: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited txtName: TEdit
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited txtUri: TButtonedEdit
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited txtUserName: TEdit
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited txtPassword: TEdit
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited cboSourceType: TComboBox
            StyleElements = [seFont, seClient, seBorder]
          end
        end
        inherited Panel3: TPanel
          Width = 851
          Height = 129
          StyleElements = [seFont, seClient, seBorder]
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
          StyleElements = [seFont, seClient, seBorder]
          inherited Label4: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label7: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label9: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label10: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited Label11: TLabel
            StyleElements = [seFont, seClient, seBorder]
          end
          inherited spAutoCloseDelay: TSpinEdit
            Height = 24
            StyleElements = [seFont, seClient, seBorder]
            ExplicitHeight = 24
          end
          inherited cboLogLevel: TComboBox
            StyleElements = [seFont, seClient, seBorder]
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
