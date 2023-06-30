inherited DPMGroupEditViewFrame: TDPMGroupEditViewFrame
  Width = 1095
  Height = 670
  ParentFont = False
  ExplicitWidth = 1095
  ExplicitHeight = 670
  inherited ContentPanel: TPanel
    Width = 1095
    Height = 670
    ExplicitWidth = 1095
    ExplicitHeight = 670
    inherited Splitter2: TSplitter
      Left = 587
      Height = 670
      ExplicitLeft = 216
      ExplicitHeight = 341
    end
    inherited PackageListPanel: TPanel
      Width = 587
      Height = 670
      ExplicitWidth = 587
      ExplicitHeight = 670
    end
    inherited DetailPanel: TPanel
      Left = 595
      Height = 670
      ExplicitLeft = 595
      ExplicitHeight = 670
      inline PackageDetailsFrame: TGroupPackageDetailsFrame
        Left = 0
        Top = 0
        Width = 500
        Height = 670
        Align = alClient
        Color = clBtnFace
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitWidth = 500
        ExplicitHeight = 670
        inherited sbPackageDetails: TScrollBox
          Width = 500
          Height = 670
          ExplicitWidth = 500
          ExplicitHeight = 670
          inherited DetailsSplitter: TSplitter
            Width = 500
            ExplicitWidth = 410
          end
          inherited pnlPackageId: TPanel
            Width = 500
            ExplicitWidth = 500
          end
          inherited pnlGridHost: TPanel
            Width = 500
            inherited pnlVersion: TPanel
              Width = 500
              ExplicitWidth = 500
              inherited SpeedButton1: TSpeedButton
                Left = 467
              end
              inherited SpeedButton2: TSpeedButton
                Left = 431
              end
              inherited Label1: TLabel
                Left = 236
                ExplicitLeft = 236
              end
              inherited ComboBox1: TComboBox
                Left = 278
                ExplicitLeft = 278
              end
            end
          end
        end
      end
    end
  end
end
