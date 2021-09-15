inherited DPMEditViewFrame: TDPMEditViewFrame
  Width = 1001
  Height = 767
  Margins.Top = 0
  Constraints.MinWidth = 320
  DoubleBuffered = True
  ParentDoubleBuffered = False
  ParentFont = False
  ExplicitWidth = 1001
  ExplicitHeight = 767
  inherited ContentPanel: TPanel
    Width = 1001
    Height = 767
    ExplicitWidth = 1001
    ExplicitHeight = 767
    inherited Splitter2: TSplitter
      Left = 528
      Height = 767
      ExplicitLeft = 216
      ExplicitHeight = 341
    end
    inherited PackageListPanel: TPanel
      Width = 528
      Height = 767
      ExplicitWidth = 528
      ExplicitHeight = 767
    end
    inherited DetailPanel: TPanel
      Left = 536
      Width = 465
      Height = 767
      ExplicitLeft = 536
      ExplicitWidth = 465
      ExplicitHeight = 767
      inline PackageDetailsFrame: TPackageDetailsFrame
        Left = 0
        Top = 0
        Width = 465
        Height = 767
        Align = alClient
        DoubleBuffered = True
        Color = clWindow
        ParentBackground = False
        ParentColor = False
        ParentDoubleBuffered = False
        TabOrder = 0
        ExplicitWidth = 465
        ExplicitHeight = 767
        inherited sbPackageDetails: TScrollBox
          Width = 465
          Height = 767
          ExplicitWidth = 465
          ExplicitHeight = 767
          inherited pnlPackageId: TPanel
            Width = 465
            ExplicitWidth = 465
          end
          inherited pnlInstalled: TPanel
            Width = 465
            ExplicitWidth = 465
            inherited txtInstalledVersion: TEdit
              Width = 305
              ExplicitWidth = 305
            end
            inherited btnUninstall: TButton
              Left = 386
              ExplicitLeft = 386
            end
          end
          inherited pnlVersion: TPanel
            Width = 465
            ExplicitWidth = 465
            inherited cboVersions: TComboBox
              Width = 305
              ExplicitWidth = 305
            end
            inherited btnInstallOrUpdate: TButton
              Left = 386
              ExplicitLeft = 386
            end
          end
        end
      end
    end
  end
end
