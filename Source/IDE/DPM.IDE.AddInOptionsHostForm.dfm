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
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inline DPMOptionsFrame: TDPMOptionsFrame
    Left = 0
    Top = 0
    Width = 720
    Height = 504
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 720
    inherited pgOptions: TPageControl
      Width = 720
      inherited tsSources: TTabSheet
        ExplicitLeft = 0
        ExplicitWidth = 712
        inherited Panel1: TPanel
          Width = 712
          ExplicitWidth = 712
        end
        inherited Panel2: TPanel
          Width = 712
        end
        inherited Panel3: TPanel
          Width = 712
          ExplicitWidth = 712
          inherited lvSources: TListView
            Width = 712
            ExplicitWidth = 712
          end
        end
      end
      inherited tsIDEOptions: TTabSheet
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inherited pnlIDEOptions: TPanel
          Width = 712
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
