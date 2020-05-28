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
    inherited Panel1: TPanel
      Width = 720
      inherited SpeedButton1: TSpeedButton
        Left = 539
      end
      inherited SpeedButton2: TSpeedButton
        Left = 584
      end
      inherited SpeedButton3: TSpeedButton
        Left = 629
      end
      inherited SpeedButton4: TSpeedButton
        Left = 674
      end
    end
    inherited Panel2: TPanel
      Top = 254
      Width = 720
    end
    inherited Panel3: TPanel
      Width = 720
      Height = 165
      inherited lvSources: TListView
        Width = 720
        Height = 165
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
