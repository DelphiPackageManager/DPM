object DPMOptionsHostForm: TDPMOptionsHostForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DPM Options'
  ClientHeight = 549
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 13
  inline DPMOptionsFrame: TDPMOptionsFrame
    Left = 0
    Top = 0
    Width = 720
    Height = 504
    Align = alClient
    Color = clWindow
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitWidth = 720
    inherited pgOptions: TPageControl
      Width = 720
      ExplicitWidth = 716
      ExplicitHeight = 503
      inherited tsSources: TTabSheet
        ExplicitWidth = 712
        inherited Panel1: TPanel
          Width = 712
          ExplicitWidth = 708
          inherited lblPackageSources: TLabel
            Width = 108
            Height = 13
            ExplicitWidth = 108
            ExplicitHeight = 13
          end
          inherited Label3: TLabel
            Left = 16
            Width = 109
            Height = 13
            ExplicitLeft = 16
            ExplicitWidth = 109
            ExplicitHeight = 13
          end
          inherited SpeedButton1: TSpeedButton
            Left = 475
          end
          inherited SpeedButton2: TSpeedButton
            Left = 520
          end
          inherited SpeedButton3: TSpeedButton
            Left = 565
          end
          inherited SpeedButton4: TSpeedButton
            Left = 610
          end
        end
        inherited Panel2: TPanel
          Width = 712
          ExplicitTop = 258
          ExplicitWidth = 708
          inherited Label1: TLabel
            Width = 70
            Height = 13
            ExplicitWidth = 70
            ExplicitHeight = 13
          end
          inherited Label2: TLabel
            Width = 94
            Height = 13
            ExplicitWidth = 94
            ExplicitHeight = 13
          end
          inherited Label5: TLabel
            Width = 92
            Height = 13
            ExplicitWidth = 92
            ExplicitHeight = 13
          end
          inherited Label6: TLabel
            Width = 158
            Height = 13
            ExplicitWidth = 158
            ExplicitHeight = 13
          end
          inherited Label8: TLabel
            Width = 67
            Height = 13
            ExplicitWidth = 67
            ExplicitHeight = 13
          end
          inherited cboSourceType: TComboBox
            Height = 21
            ExplicitHeight = 21
          end
        end
        inherited Panel3: TPanel
          Width = 712
          ExplicitWidth = 708
          ExplicitHeight = 169
          inherited lvSources: TListView
            Width = 712
            ParentColor = True
            ExplicitWidth = 708
            ExplicitHeight = 169
          end
        end
      end
      inherited tsIDEOptions: TTabSheet
        ExplicitWidth = 712
        inherited pnlIDEOptions: TPanel
          Width = 712
          inherited Label4: TLabel
            Width = 65
            Height = 13
            ExplicitWidth = 65
            ExplicitHeight = 13
          end
          inherited Label7: TLabel
            Width = 95
            Height = 13
            ExplicitWidth = 95
            ExplicitHeight = 13
          end
          inherited Label9: TLabel
            Width = 132
            Height = 13
            ExplicitWidth = 132
            ExplicitHeight = 13
          end
          inherited Label10: TLabel
            Width = 79
            Height = 13
            ExplicitWidth = 79
            ExplicitHeight = 13
          end
          inherited Label11: TLabel
            Width = 239
            Height = 13
            ExplicitWidth = 239
            ExplicitHeight = 13
          end
          inherited cboLogLevel: TComboBox
            Height = 21
            ExplicitHeight = 21
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 504
    Width = 720
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 503
    ExplicitWidth = 716
    object btnCancel: TButton
      Left = 629
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 528
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
end
