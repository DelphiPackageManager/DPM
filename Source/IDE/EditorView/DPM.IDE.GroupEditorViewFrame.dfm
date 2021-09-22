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
      Left = 677
      Height = 670
      ExplicitLeft = 216
      ExplicitHeight = 341
    end
    inherited PackageListPanel: TPanel
      Width = 677
      Height = 670
      ExplicitWidth = 677
      ExplicitHeight = 670
    end
    inherited DetailPanel: TPanel
      Left = 685
      Height = 670
      ExplicitLeft = 685
      ExplicitHeight = 670
      inline PackageDetailsFrame: TGroupPackageDetailsFrame
        Left = 0
        Top = 0
        Width = 410
        Height = 670
        Align = alClient
        Color = clBtnFace
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitWidth = 410
        ExplicitHeight = 670
        inherited sbPackageDetails: TScrollBox
          Width = 410
          Height = 670
          ExplicitWidth = 410
          ExplicitHeight = 670
          inherited DetailsSplitter: TSplitter
            Width = 410
            ExplicitWidth = 410
          end
          inherited pnlPackageId: TPanel
            Width = 410
            ExplicitWidth = 410
          end
          inherited pnlGridHost: TPanel
            Width = 410
            ExplicitWidth = 410
            inherited pnlInstalled: TPanel
              Width = 410
              ExplicitWidth = 410
              inherited txtInstalledVersion: TEdit
                Width = 250
                ExplicitWidth = 250
              end
              inherited btnUninstall: TButton
                Left = 331
                ExplicitLeft = 331
              end
            end
            inherited pnlVersion: TPanel
              Width = 410
              ExplicitWidth = 410
              inherited cboVersions: TComboBox
                Width = 250
                ExplicitWidth = 250
              end
              inherited btnInstall: TButton
                Left = 331
                ExplicitLeft = 331
              end
            end
          end
        end
      end
    end
  end
end
