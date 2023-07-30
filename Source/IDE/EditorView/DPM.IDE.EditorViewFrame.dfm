object DPMEditViewFrame: TDPMEditViewFrame
  Left = 0
  Top = 0
  Width = 1010
  Height = 581
  Color = clBtnFace
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object ContentPanel: TPanel
    Left = 0
    Top = 0
    Width = 1010
    Height = 581
    Align = alClient
    BevelEdges = [beTop]
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    Constraints.MinWidth = 320
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 502
      Top = 0
      Width = 8
      Height = 581
      Align = alRight
      MinSize = 300
      ResizeStyle = rsUpdate
      ExplicitLeft = 592
    end
    object PackageListPanel: TPanel
      Left = 0
      Top = 0
      Width = 502
      Height = 581
      Align = alClient
      BevelEdges = [beRight]
      BevelOuter = bvNone
      Caption = 'PackageListPanel'
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
    end
    object DetailPanel: TPanel
      Left = 510
      Top = 0
      Width = 500
      Height = 581
      Align = alRight
      BevelEdges = [beRight]
      BevelOuter = bvNone
      Caption = 'DetailPanel'
      Constraints.MinWidth = 400
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      inline PackageDetailsFrame: TGroupPackageDetailsFrame
        Left = 0
        Top = 0
        Width = 500
        Height = 581
        Align = alClient
        Color = clBtnFace
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitWidth = 500
        ExplicitHeight = 581
        inherited sbPackageDetails: TScrollBox
          Width = 500
          Height = 581
          ExplicitWidth = 500
          ExplicitHeight = 581
          inherited DetailsSplitter: TSplitter
            Width = 500
            ExplicitWidth = 500
          end
          inherited pnlPackageId: TPanel
            Width = 500
            ExplicitWidth = 500
          end
          inherited pnlGridHost: TPanel
            Width = 500
            ExplicitWidth = 500
            inherited pnlVersion: TPanel
              Width = 500
              ExplicitWidth = 500
              inherited btnInstallAll: TSpeedButton
                Left = 395
                ExplicitLeft = 395
              end
              inherited btnUpgradeAll: TSpeedButton
                Left = 431
                ExplicitLeft = 431
              end
              inherited btnUninstallAll: TSpeedButton
                Left = 467
                ExplicitLeft = 467
              end
            end
          end
        end
      end
    end
  end
  object platformChangeDetectTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = platformChangeDetectTimerTimer
    Left = 318
    Top = 408
  end
  object ActivityTimer: TTimer
    Enabled = False
    Interval = 350
    OnTimer = ActivityTimerTimer
    Left = 320
    Top = 480
  end
end
