inherited DPMEditViewFrame: TDPMEditViewFrame
  Width = 934
  Height = 767
  Margins.Top = 0
  Constraints.MinWidth = 320
  DoubleBuffered = True
  ParentDoubleBuffered = False
  ParentFont = False
  ExplicitWidth = 934
  ExplicitHeight = 767
  object ContentPanel: TPanel
    Left = 0
    Top = 0
    Width = 934
    Height = 767
    Align = alClient
    BevelEdges = [beTop]
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    Constraints.MinWidth = 320
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 490
      Top = 0
      Width = 8
      Height = 767
      Align = alRight
      MinSize = 300
      ResizeStyle = rsUpdate
      ExplicitLeft = 532
      ExplicitHeight = 556
    end
    object PackageListPanel: TPanel
      Left = 0
      Top = 0
      Width = 490
      Height = 767
      Align = alClient
      BevelEdges = [beRight]
      BevelOuter = bvNone
      Caption = 'PackageListPanel'
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
    end
    inline PackageDetailsFrame: TPackageDetailsFrame
      Left = 498
      Top = 0
      Width = 436
      Height = 767
      Align = alRight
      Constraints.MinWidth = 300
      DoubleBuffered = True
      Color = clWindow
      ParentBackground = False
      ParentColor = False
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitLeft = 498
      ExplicitHeight = 767
      inherited sbPackageDetails: TScrollBox
        Height = 767
        ExplicitHeight = 767
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
end
