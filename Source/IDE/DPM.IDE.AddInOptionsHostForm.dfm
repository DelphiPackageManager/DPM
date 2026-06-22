object DPMOptionsHostForm: TDPMOptionsHostForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DPM Options'
  ClientHeight = 545
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inline DPMOptionsFrame: TDPMOptionsFrame
    Left = 0
    Top = 0
    Width = 726
    Height = 500
    Align = alClient
    Color = clWindow
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitWidth = 726
    ExplicitHeight = 500
    inherited pgOptions: TPageControl
      Width = 726
      Height = 500
      ExplicitWidth = 726
      ExplicitHeight = 500
      inherited tsSources: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 30
        ExplicitWidth = 718
        ExplicitHeight = 466
        inherited Panel1: TPanel
          Width = 718
          ExplicitWidth = 718
          inherited btnAdd: TSpeedButton
            Left = 481
            ExplicitLeft = 475
          end
          inherited btnRemove: TSpeedButton
            Left = 526
            ExplicitLeft = 520
          end
          inherited btnUp: TSpeedButton
            Left = 571
            ExplicitLeft = 565
          end
          inherited btnDown: TSpeedButton
            Left = 616
            ExplicitLeft = 610
          end
        end
        inherited Panel2: TPanel
          Top = 255
          Width = 718
          ExplicitTop = 255
          ExplicitWidth = 718
        end
        inherited Panel3: TPanel
          Width = 718
          Height = 166
          ExplicitWidth = 718
          ExplicitHeight = 166
          inherited lvSources: TListView
            Width = 718
            Height = 166
            ParentColor = True
            ExplicitWidth = 718
            ExplicitHeight = 166
          end
        end
      end
      inherited tsIDEOptions: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 30
        ExplicitWidth = 660
        ExplicitHeight = 470
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 500
    Width = 726
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      726
      45)
    object btnCancel: TButton
      Left = 629
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 524
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
end
