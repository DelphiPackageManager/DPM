object DSpecCreatorForm: TDSpecCreatorForm
  Left = 0
  Top = 0
  Caption = '.dspec Creator'
  ClientHeight = 558
  ClientWidth = 871
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 871
    Height = 558
    ActivePage = tsInfo
    Align = alClient
    TabOrder = 0
    object tsInfo: TTabSheet
      Caption = 'Package Info'
      object lblId: TLabel
        Left = 70
        Top = 32
        Width = 13
        Height = 15
        Alignment = taRightJustify
        Caption = 'Id:'
      end
      object lblVersion: TLabel
        Left = 42
        Top = 79
        Width = 41
        Height = 15
        Alignment = taRightJustify
        Caption = 'Version:'
      end
      object lblDescription: TLabel
        Left = 20
        Top = 129
        Width = 63
        Height = 15
        Alignment = taRightJustify
        Caption = 'Description:'
      end
      object lblProjectURL: TLabel
        Left = 19
        Top = 290
        Width = 64
        Height = 15
        Alignment = taRightJustify
        Caption = 'Project URL:'
      end
      object lblRepositoryURL: TLabel
        Left = 0
        Top = 319
        Width = 83
        Height = 15
        Alignment = taRightJustify
        Caption = 'Repository URL:'
      end
      object Label13: TLabel
        Left = 36
        Top = 348
        Width = 47
        Height = 15
        Alignment = taRightJustify
        Caption = 'Commit:'
      end
      object lblLicense: TLabel
        Left = 41
        Top = 377
        Width = 42
        Height = 15
        Alignment = taRightJustify
        Caption = 'License:'
      end
      object lblTags: TLabel
        Left = 56
        Top = 426
        Width = 27
        Height = 15
        Alignment = taRightJustify
        Caption = 'Tags:'
      end
      object lblAuthor: TLabel
        Left = 43
        Top = 235
        Width = 40
        Height = 15
        Alignment = taRightJustify
        Caption = 'Author:'
      end
      object lblSPDX: TLabel
        Left = 89
        Top = 402
        Width = 120
        Height = 15
        Hint = 'https://spdx.org/licenses'
        Caption = 'SPDX License Identifier'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 12940065
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        StyleElements = [seClient, seBorder]
        OnClick = UriLabelClick
        OnMouseEnter = UriLabelMouseEnter
        OnMouseLeave = UriLabelMouseLeave
      end
      object Label4: TLabel
        Left = 89
        Top = 452
        Width = 113
        Height = 15
        Caption = 'Space separated tags '
        Enabled = False
      end
      object lblPackageId: TLabel
        Left = 88
        Top = 55
        Width = 363
        Height = 15
        Caption = 
          'Package Id in the format Org/User . PackageName - e.g VSoft.DUni' +
          'tX'
        Enabled = False
      end
      object Label5: TLabel
        Left = 89
        Top = 104
        Width = 258
        Height = 15
        Hint = 'https://semver.org/'
        Caption = 'Package version using Semantic Version notation'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 12940065
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
        OnClick = UriLabelClick
        OnMouseEnter = UriLabelMouseEnter
        OnMouseLeave = UriLabelMouseLeave
      end
      object lblCopyright: TLabel
        Left = 24
        Top = 263
        Width = 59
        Height = 15
        Alignment = taRightJustify
        Caption = 'Copyright :'
      end
      object Label14: TLabel
        Left = 37
        Top = 476
        Width = 46
        Height = 15
        Alignment = taRightJustify
        Caption = 'Readme:'
      end
      object edtId: TEdit
        Left = 89
        Top = 29
        Width = 376
        Height = 23
        TabOrder = 0
        OnChange = edtIdChange
      end
      object edtVersion: TEdit
        Left = 89
        Top = 76
        Width = 376
        Height = 23
        TabOrder = 1
        OnChange = edtVersionChange
        OnExit = edtVersionExit
      end
      object mmoDescription: TMemo
        Left = 89
        Top = 126
        Width = 376
        Height = 89
        TabOrder = 2
        OnChange = mmoDescriptionChange
      end
      object edtProjectURL: TEdit
        Left = 89
        Top = 287
        Width = 376
        Height = 23
        TabOrder = 5
        OnChange = edtProjectURLChange
      end
      object edtRepositoryURL: TEdit
        Left = 89
        Top = 316
        Width = 376
        Height = 23
        TabOrder = 6
        OnChange = edtRepositoryURLChange
      end
      object cboLicense: TComboBox
        Left = 89
        Top = 374
        Width = 376
        Height = 23
        TabOrder = 7
        OnChange = cboLicenseChange
        Items.Strings = (
          'Apache 2.0'
          'GNU General Public License v3.0'
          'GNU General Public License v2.0'
          'MIT License'
          'BSD 2-Clause "Simplified" License'
          'BSD 3-Clause "New" or "Revised" License'
          'Boost Software License 1.0'
          'Creative Commons Zero v1.0 Universal'
          'Eclipse Public License 2.0'
          'GNU Affero General Public License v3.0'
          'GNU Lesser General Public License v2.1'
          'Mozilla Public License 2.0'
          'The Unlicense')
      end
      object edtTags: TEdit
        Left = 89
        Top = 423
        Width = 376
        Height = 23
        TabOrder = 8
        OnChange = edtTagsChange
      end
      object edtAuthor: TEdit
        Left = 89
        Top = 232
        Width = 376
        Height = 23
        TabOrder = 3
        OnChange = edtAuthorChange
      end
      object pnlIcon: TPanel
        Left = 488
        Top = 23
        Width = 128
        Height = 128
        BevelKind = bkFlat
        Caption = 'Select Icon...'
        TabOrder = 9
        object ImgIcon: TImage
          Left = 1
          Top = 1
          Width = 122
          Height = 122
          Align = alClient
          Center = True
          Proportional = True
          Stretch = True
          OnClick = ImgIconClick
        end
      end
      object edtCopyright: TEdit
        Left = 89
        Top = 260
        Width = 376
        Height = 23
        TabOrder = 4
        OnChange = edtCopyrightChange
      end
      object edtReadme: TEdit
        Left = 89
        Top = 473
        Width = 376
        Height = 23
        TabOrder = 10
        OnChange = edtReadmeChange
      end
    end
    object tsPlatforms: TTabSheet
      Caption = 'Platforms'
      ImageIndex = 1
      object lblTemplate: TLabel
        Left = 256
        Top = 252
        Width = 49
        Height = 15
        Caption = 'Template'
        Enabled = False
      end
      object lblCompilers: TLabel
        Left = 72
        Top = 7
        Width = 95
        Height = 15
        Caption = 'Compiler Versions'
      end
      object lblPlatform: TLabel
        Left = 256
        Top = 7
        Width = 46
        Height = 15
        Caption = 'Platform'
        Enabled = False
      end
      object clbCompilers: TCheckListBox
        Left = 72
        Top = 28
        Width = 161
        Height = 417
        ItemHeight = 17
        Items.Strings = (
          'XE2'
          'XE3'
          'XE4'
          'XE5'
          'XE6'
          'XE7'
          'XE8'
          '10.0'
          '10.1'
          '10.2'
          '10.3'
          '10.4'
          '11.0'
          '12.0'
          '13.0')
        PopupMenu = pmCompilers
        TabOrder = 0
        OnClick = clbCompilersClick
        OnKeyPress = clbCompilersKeyPress
      end
      object cboTemplate: TComboBox
        Left = 256
        Top = 273
        Width = 161
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        OnChange = cboTemplateChange
      end
      object clbPlatforms: TCheckListBox
        Left = 256
        Top = 28
        Width = 161
        Height = 218
        DoubleBuffered = True
        Enabled = False
        ItemHeight = 17
        Items.Strings = (
          'Win32'
          'Win64'
          'OSX32'
          'OSX64'
          'OSXARM64'
          'Android'
          'Android64'
          'iOS32'
          'iOS64'
          'iOSSimulator'
          'LinuxIntel64')
        ParentDoubleBuffered = False
        PopupMenu = pmPlatforms
        TabOrder = 2
        OnClickCheck = clbPlatformsClickCheck
      end
      object VariablesList: TValueListEditor
        Left = 256
        Top = 308
        Width = 297
        Height = 137
        DoubleBuffered = True
        Enabled = False
        KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
        ParentDoubleBuffered = False
        TabOrder = 3
        TitleCaptions.Strings = (
          'Variable Name'
          'Value')
        OnStringsChange = VariablesListStringsChange
        ColWidths = (
          150
          141)
      end
    end
    object tsTemplates: TTabSheet
      Caption = 'Templates'
      ImageIndex = 2
      object Splitter1: TSplitter
        Left = 330
        Top = 0
        Width = 4
        Height = 528
        ResizeStyle = rsUpdate
      end
      object CardPanel: TCardPanel
        Left = 334
        Top = 0
        Width = 529
        Height = 528
        Align = alClient
        ActiveCard = crdSearchPathItem
        Caption = 'CardPanel'
        TabOrder = 0
        object crdSource: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Source/Lib/File'
          CardIndex = 0
          TabOrder = 0
          DesignSize = (
            527
            526)
          object lblSrc: TLabel
            Left = 16
            Top = 40
            Width = 19
            Height = 15
            Caption = 'Src:'
          end
          object lblDest: TLabel
            Left = 16
            Top = 113
            Width = 26
            Height = 15
            Caption = 'Dest:'
          end
          object lblSourceItemHeader: TLabel
            Left = 8
            Top = 8
            Width = 62
            Height = 15
            Caption = 'Source Files'
          end
          object Label10: TLabel
            Left = 16
            Top = 163
            Width = 79
            Height = 15
            Caption = 'Excluded Items'
          end
          object edtFileEntrySource: TEdit
            Left = 16
            Top = 61
            Width = 365
            Height = 23
            CustomHint = BalloonHint1
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtFileEntrySourceChange
          end
          object chkFileEntryFlatten: TCheckBox
            Left = 16
            Top = 90
            Width = 97
            Height = 17
            Caption = 'Flatten'
            TabOrder = 1
          end
          object edtFileEntryDest: TEdit
            Left = 16
            Top = 134
            Width = 365
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnChange = edtFileEntryDestChange
          end
          object lbFileEntryExclude: TListBox
            Left = 16
            Top = 182
            Width = 365
            Height = 97
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 15
            TabOrder = 3
          end
          object btnAddExclude: TButton
            Left = 96
            Top = 293
            Width = 75
            Height = 25
            Caption = 'Add Exclude'
            TabOrder = 4
            OnClick = btnAddExcludeClick
          end
          object btnDeleteExclude: TButton
            Left = 225
            Top = 293
            Width = 88
            Height = 25
            Caption = 'Delete Exclude'
            TabOrder = 5
            OnClick = btnDeleteExcludeClick
          end
        end
        object crdSearchPathItem: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'SearchPath Item'
          CardIndex = 1
          TabOrder = 1
          object lblSearchPaths: TLabel
            Left = 8
            Top = 8
            Width = 92
            Height = 15
            Caption = 'Search Path Entry'
          end
          object Label9: TLabel
            Left = 16
            Top = 40
            Width = 24
            Height = 15
            Caption = 'Path'
          end
          object edtSearchPath: TEdit
            Left = 16
            Top = 62
            Width = 401
            Height = 23
            TabOrder = 0
            OnChange = edtSearchPathChange
          end
        end
        object crdBuild: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Build Item'
          CardIndex = 2
          TabOrder = 2
          DesignSize = (
            527
            526)
          object lblBuild: TLabel
            Left = 8
            Top = 8
            Width = 27
            Height = 15
            Caption = 'Build'
          end
          object lblBuildId: TLabel
            Left = 24
            Top = 37
            Width = 40
            Height = 15
            Caption = 'BuildId:'
          end
          object lblProject: TLabel
            Left = 24
            Top = 85
            Width = 40
            Height = 15
            Caption = 'Project:'
          end
          object lblConfiguration: TLabel
            Left = 24
            Top = 135
            Width = 77
            Height = 15
            Caption = 'Configuration:'
          end
          object edtBuildId: TEdit
            Left = 24
            Top = 56
            Width = 414
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = edtBuildIdChange
          end
          object edtProject: TEdit
            Left = 24
            Top = 106
            Width = 414
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = edtProjectChange
          end
          object edtConfiguration: TEdit
            Left = 24
            Top = 156
            Width = 414
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnChange = edtConfigurationChange
          end
          object chkBuildForDesign: TCheckBox
            Left = 24
            Top = 193
            Width = 113
            Height = 17
            Caption = 'Build For Design'
            TabOrder = 3
            OnClick = chkBuildForDesignClick
          end
          object chkDesignOnly: TCheckBox
            Left = 24
            Top = 223
            Width = 113
            Height = 17
            Caption = 'Design Only'
            TabOrder = 4
            OnClick = chkDesignOnlyClick
          end
        end
        object crdRuntimeOrDesignBpl: TCard
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 521
          Height = 520
          Caption = 'Runtime Or DesignTime'
          CardIndex = 3
          TabOrder = 3
          DesignSize = (
            521
            520)
          object lblRuntime: TLabel
            Left = 8
            Top = 8
            Width = 45
            Height = 15
            Caption = 'Runtime'
          end
          object lblRuntimeBuildId: TLabel
            Left = 10
            Top = 115
            Width = 46
            Height = 15
            Alignment = taRightJustify
            Caption = 'Build Id :'
          end
          object lblRuntimeSrc: TLabel
            Left = 34
            Top = 40
            Width = 22
            Height = 15
            Alignment = taRightJustify
            Caption = 'Src :'
          end
          object edtBPLEntryBuildId: TEdit
            Left = 62
            Top = 112
            Width = 416
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            OnChange = edtBPLEntryBuildIdChange
          end
          object edtBPLEntrySrc: TEdit
            Left = 62
            Top = 37
            Width = 416
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtBPLEntrySrcChange
          end
          object chkCopyLocal: TCheckBox
            Left = 62
            Top = 66
            Width = 97
            Height = 17
            Caption = 'Copy Local'
            TabOrder = 1
            OnClick = chkCopyLocalClick
          end
          object chkInstall: TCheckBox
            Left = 62
            Top = 89
            Width = 97
            Height = 17
            Caption = 'Install'
            TabOrder = 2
            OnClick = chkDesignInstallClick
          end
        end
        object crdDependency: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Dependency'
          CardIndex = 4
          TabOrder = 4
          DesignSize = (
            527
            526)
          object Label1: TLabel
            Left = 16
            Top = 95
            Width = 80
            Height = 15
            Caption = 'Version Range :'
          end
          object lblDependencyId: TLabel
            Left = 16
            Top = 40
            Width = 16
            Height = 15
            Caption = 'Id :'
          end
          object Label8: TLabel
            Left = 8
            Top = 8
            Width = 66
            Height = 15
            Caption = 'Dependency'
          end
          object edtDependencyId: TEdit
            Left = 16
            Top = 61
            Width = 405
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = edtDependencyIdChange
          end
          object edtDependencyVersion: TEdit
            Left = 16
            Top = 116
            Width = 405
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = edtDependencyVersionChange
          end
        end
        object crdTemplate: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Templates'
          CardIndex = 5
          TabOrder = 5
          object lblTemplateName: TLabel
            Left = 16
            Top = 40
            Width = 32
            Height = 15
            Caption = 'Name'
          end
          object Label11: TLabel
            Left = 8
            Top = 8
            Width = 49
            Height = 15
            Caption = 'Template'
          end
          object edtTemplateName: TEdit
            Left = 16
            Top = 61
            Width = 401
            Height = 23
            TabOrder = 0
            OnChange = edtTemplateNameChange
          end
        end
        object crdBuildHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Build Items Heading'
          CardIndex = 6
          TabOrder = 6
          object lblBuildHeading: TLabel
            Left = 8
            Top = 8
            Width = 59
            Height = 15
            Caption = 'Build Items'
          end
          object lblBuildDescription: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 
              'Build Items describe how to build (on install) a Runtime or Desi' +
              'gn time bpl.'
            Enabled = False
            WordWrap = True
          end
        end
        object crdSearchPathHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'SearchPath Heading'
          CardIndex = 7
          TabOrder = 7
          object lblSearchPathsDescription: TLabel
            Left = 8
            Top = 32
            Width = 505
            Height = 292
            AutoSize = False
            Caption = 
              'Add Search paths from the package that wll be added to the Proje' +
              'ct'#39's Search Path when the package is installed.'
            Enabled = False
            WordWrap = True
          end
          object lblSearchPathsHeading: TLabel
            Left = 8
            Top = 8
            Width = 67
            Height = 15
            Caption = 'Search Paths'
          end
        end
        object crdSourceHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Source Items Heading'
          CardIndex = 8
          TabOrder = 8
          object lblSourceItemsHeading: TLabel
            Left = 8
            Top = 8
            Width = 68
            Height = 15
            Caption = 'Source Items'
          end
          object lblSourceItemsDescription: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 'Define source files to include in the dpm package'
            Enabled = False
            WordWrap = True
          end
        end
        object crdDependenciesHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Dependencies '
          CardIndex = 9
          TabOrder = 9
          object Label6: TLabel
            Left = 8
            Top = 8
            Width = 74
            Height = 15
            Caption = 'Dependencies'
          end
          object Label7: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 
              'Dependencies define the other packages that this package, so tha' +
              't DPM knows what else to install when installing this package.'
            Enabled = False
            WordWrap = True
          end
        end
        object crdLibEntriesHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Lib Entries Heading'
          CardIndex = 10
          TabOrder = 10
          object lblLibFilesHeading: TLabel
            Left = 8
            Top = 8
            Width = 42
            Height = 15
            Caption = 'Lib Files'
          end
          object lblLibFilesDescription: TLabel
            Left = 8
            Top = 32
            Width = 505
            Height = 292
            AutoSize = False
            Caption = 'Add Lib Entries for precompiled dcu/dcp/obj files'
            Enabled = False
            WordWrap = True
          end
        end
        object crdFileEntriesHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'File Entries Heading'
          CardIndex = 11
          TabOrder = 11
          object lblFileEntriesHeading: TLabel
            Left = 8
            Top = 8
            Width = 56
            Height = 15
            Caption = 'File Entries'
          end
          object lblFileEntriesDescription: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 
              'Add File Entries to define non source or binaries to include in ' +
              'the package (eg, images)'
            Enabled = False
            WordWrap = True
          end
        end
        object crdDesignHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Design Heading'
          CardIndex = 12
          TabOrder = 12
          object lblDesignHeading: TLabel
            Left = 8
            Top = 8
            Width = 68
            Height = 15
            Caption = 'Design Items'
          end
          object lblDesignDescription: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 
              'Define Design time packages to be installed in the IDE when this' +
              ' package is installed.'
            Enabled = False
            WordWrap = True
          end
        end
        object crdRuntimeHeading: TCard
          Left = 1
          Top = 1
          Width = 527
          Height = 526
          Caption = 'Runtime Heading'
          CardIndex = 13
          TabOrder = 13
          object lblRuntimeHeading: TLabel
            Left = 8
            Top = 8
            Width = 97
            Height = 15
            Caption = 'Runtime Packages'
          end
          object lblRuntimeDescription: TLabel
            Left = 8
            Top = 32
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 'Define Runtime packages (bpl) for projects that use them'
            Enabled = False
            WordWrap = True
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 330
        Height = 528
        Align = alLeft
        Caption = 'Panel2'
        ShowCaption = False
        TabOrder = 1
        DesignSize = (
          330
          528)
        object lblTemplateView: TLabel
          Left = 3
          Top = 3
          Width = 77
          Height = 15
          Caption = 'Template View'
        end
        object Label3: TLabel
          Left = 3
          Top = 469
          Width = 161
          Height = 15
          Anchors = [akLeft, akBottom]
          Caption = 'Right click to add/delete items'
          Enabled = False
        end
        object tvTemplates: TTreeView
          Left = 3
          Top = 24
          Width = 319
          Height = 439
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoExpand = True
          DoubleBuffered = True
          HideSelection = False
          HotTrack = True
          Images = ImageList1
          Indent = 19
          ParentDoubleBuffered = False
          PopupMenu = PopupMenu
          RightClickSelect = True
          RowSelect = True
          TabOrder = 0
          OnChange = tvTemplatesChange
          OnCollapsing = tvTemplatesCollapsing
          OnContextPopup = tvTemplatesContextPopup
          OnCreateNodeClass = tvTemplatesCreateNodeClass
          OnEdited = tvTemplatesEdited
          OnEditing = tvTemplatesEditing
        end
        object btnAddTemplate: TButton
          Left = 3
          Top = 490
          Width = 86
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Add Template'
          TabOrder = 1
          OnClick = btnAddTemplateClick
        end
        object btnDeleteTemplate: TButton
          Left = 95
          Top = 490
          Width = 98
          Height = 25
          Action = actDeleteTemplate
          Anchors = [akLeft, akBottom]
          TabOrder = 2
        end
        object btnDuplicateTemplate: TButton
          Left = 200
          Top = 490
          Width = 122
          Height = 25
          Action = actDuplicateTemplate
          Anchors = [akLeft, akBottom]
          TabOrder = 3
        end
      end
    end
    object tsGenerate: TTabSheet
      Caption = 'Generate'
      ImageIndex = 3
      object GridPanel1: TGridPanel
        Left = 0
        Top = 0
        Width = 863
        Height = 528
        Align = alClient
        Caption = 'GridPanel1'
        ColumnCollection = <
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Panel1
            Row = 0
          end
          item
            Column = 0
            Control = Memo1
            Row = 1
          end>
        RowCollection = <
          item
            Value = 16.666666666666670000
          end
          item
            Value = 83.333333333333330000
          end>
        TabOrder = 0
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 861
          Height = 88
          Align = alClient
          TabOrder = 0
          object Label2: TLabel
            Left = 136
            Top = 24
            Width = 115
            Height = 15
            Caption = 'Package Output Path:'
          end
          object btnBuildPackages: TButton
            Left = 16
            Top = 22
            Width = 97
            Height = 25
            Caption = 'Build Packages'
            TabOrder = 0
            OnClick = btnBuildPackagesClick
          end
          object edtPackageOutputPath: TEdit
            Left = 257
            Top = 21
            Width = 354
            Height = 23
            TabOrder = 1
            OnExit = edtPackageOutputPathExit
          end
        end
        object Memo1: TMemo
          Left = 1
          Top = 89
          Width = 861
          Height = 438
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
      end
    end
    object tsLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 4
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 863
        Height = 528
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object edtRepositoryCommit: TEdit
    Left = 93
    Top = 371
    Width = 376
    Height = 23
    TabOrder = 1
    OnChange = edtRepositoryCommitChange
  end
  object MainMenu: TMainMenu
    Left = 508
    Top = 386
    object mnuFile: TMenuItem
      Caption = '&File'
      object miNew: TMenuItem
        Action = actFileNew
      end
      object miOpen: TMenuItem
        Action = actFileOpen
      end
      object mnuFileOpenSep: TMenuItem
        Caption = '-'
      end
      object miSave: TMenuItem
        Action = actFileSave
      end
      object mnuSaveAs: TMenuItem
        Action = actFileSaveAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actFileExit
      end
    end
    object miOptions: TMenuItem
      Caption = 'Options'
      OnClick = miOptionsClick
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object CreatingPackages1: TMenuItem
        Caption = 'Creating Packages'
        OnClick = CreatingPackages1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.dspec'
    Filter = 'Delphi Package Manager Spec Files|*.dspec'
    Left = 812
    Top = 386
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.dspec'
    Filter = 'Delphi Package Manager Spec Files|*.dspec'
    Left = 740
    Top = 386
  end
  object PopupMenu: TPopupMenu
    Left = 588
    Top = 386
  end
  object BalloonHint1: TBalloonHint
    Left = 757
    Top = 310
  end
  object ImageList1: TImageList
    Left = 693
    Top = 310
    Bitmap = {
      494C010107000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1A1A10053535300505050005151
      5100515151005151510051515100515151005151510051515100515151005151
      5100515151004E4E4E0047474700A9A9A9001F1F1F0043434300424242001111
      1100424242003D3D3D001818180043434300272727002C2C2C00434343001313
      1300414141004343430022222200404040000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F00E2E3E300DEDEDE00DBDB
      DB00DBDBDB00D9DADA00DADBDB00D9D9D900D7D7D700D6D6D600D5D5D500D4D4
      D400D3D3D300E0E0E000999999004949490042424200000000FF000000FF4141
      4100000000FF000000FF77787800000000FF96969600A9A9A900000000FF6B6B
      6B00000000FFCECECE003B3B3B00000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008282820076767600767676000000
      000000000000000000000000000000000000ABABAB00FCFBFB00F0EEEE00F0EF
      EF00ECEBEB00ECEBEB00E5E2E200E6E2E200E9E7E700E8E7E700EAEAEA00E8E9
      E900E6E6E600F5F4F400C6C6C6004E4E4E0042424200000000FF000000FFCACA
      CA00000000FF000000FF000000FF000000FFDBDDDD00000000FF000000FF0000
      00FFCECECE003B3B3B00000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000858585004343430087878700878787000000
      000000000000000000000000000000000000A9A9A900F7F5F500DEDADA00DAD3
      D300DAD4D400DFDCDC00D8D3D300D7D2D200D3CDCD00D6D1D100DEDBDB00DBD9
      D900DBD9D900ECEDED00BDBDBD004F4F4F001D1D1D0065656500CECECE000000
      00FF91919100848484008484840084848400BABABA00000000FF000000FFCECE
      CE003B3B3B00000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000AAAAAA00F4F2F200DAD4D400DCD5
      D500E5E3E300E9E9E900E5E3E300E3E1E100E1DFDF00DEDCDC00E0DEDE00DFDD
      DD00DEDDDD00EEEEEE00C0C0C0004F4F4F003A3A3A00DCDCDC00000000FF0000
      00FF30303000BABABA00A7A7A7001E1E1E00C2C4C400000000FFCBCBCB003B3B
      3B00000000FF000000FFDBDBDB00AFAFAF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000ABABAB00F9F7F700E0DADA00DBD5
      D500DFDADA00E1DDDD00E1DDDD00E2DDDD00E4E2E200DDDADA00DFDDDD00E0DF
      DF00E1E1E100EFF0F000C1C1C1004F4F4F0042424200000000FF000000FF0000
      00FF41414100000000FF40404000BDBDBD00000000FFCBCCCC003A3A3A00DFE0
      E0007E7E7E00363636005C5C5C002E2E2E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000AEAEAE00F1ECEC00D4CBCB00E0DA
      DA00E9E7E700E2DFDF00EBEAEA00EDEEEE00EAEAEA00E8E9E900E6E7E700E7E6
      E600E3E4E400F0F0F000C3C2C200505050001515150064646400000000FF0000
      00FF313131003F3F3F00C5C5C500000000FFCBCBCB003C3C3C00000000FF7172
      72007070700039393900AFAFAF00676767000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000076767600767676003A3A3A003A3A3A0076767600767676000000
      000000000000000000000000000000000000AEAFAF00FAF9F900DFDADA00E1DB
      DB00E6E3E300E9E8E800E1DCDC00E5E3E300E4E2E200E3E0E000E2E0E000DEDA
      DA00DDDADA00F1F1F100C5C5C5005050500042424200000000FF000000FF0000
      00FF14141400C5C5C500000000FFCFCFCF0039393900000000FF7A7A7A007A7A
      7A00000000FFDADADA001E1E1E00A1A1A1000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008787870087878700434343004343430087878700878787000000
      000000000000000000000000000000000000AFAFAF00FEFEFE00ECEAEA00E4E1
      E100E5E1E100E9E6E600DFDBDB00DED9D900E3DFDF00DFDCDC00E0DCDC00E4E3
      E300E3E2E200F2F3F300C6C7C7005050500036363600CBCBCB00000000FF0000
      00FF000000FF000000FFCECECE003B3B3B00000000FF85858500717171009C9C
      9C0067676700B9B9B90044444400000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000B2B2B200F8F6F600E8E5E500EDEB
      EB00EBEAEA00E9E6E600EEEDED00E6E4E400E4E1E100E9E8E800EBEAEA00EAEB
      EB00E7E7E700F4F4F400C8C8C800505050002121210076767600D2D2D2000000
      00FF000000FFCECECE003B3B3B00000000FF9191910062626200ACACAC005151
      5100C0C0C00041414100000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000B3B4B400F6F3F300DCD5D500E9E6
      E600ECEAEA00EAE6E600EEEDED00E8E4E400E5E1E100EBEAEA00EDEDED00EAEA
      EA00E8E8E800F5F5F500C9C9C9005050500042424200000000FF000000FF0000
      00FFCFCFCF003A3A3A00000000FF9D9D9D005F5F5F00B8B8B80049494900C8C8
      C8003D3F3F00000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007E7E7E007E7E7E0000000000000000000000
      000000000000000000000000000000000000B3B3B300FFFFFF00F3F5F800F7FB
      FD00F9FEFF00F7FDFF00F5FBFE00F5FAFE00F3FAFD00F1F7FB00F0F5F900EEF4
      F700ECF2F600F9FFFF00CBCCCC00525252002525250098989800000000FFCECE
      CE003B3B3B00000000FFA1A1A10055555500C1C1C10043434300C6C6C6003939
      3900000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000858585003A3A3A0076767600767676000000
      000000000000000000000000000000000000B5B4B400FCE6D800EFD2BD00EDD2
      BF00EACEBD00E9CCBB00E8CAB800E6C8B500E5C6B300E2C3B200E1C1AF00DFBF
      AD00DCB9A600EBCBB900CBC6C2005355550032323200CACACA00CECECE003B3B
      3B00000000FFAAAAAA0015151500000000FF6B6B6B00C8C8C8003A3A3A000000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008A8A8A0087878700878787000000
      000000000000000000000000000000000000B9B9B700EDB48F00D6885300D88C
      5B00D6885500D3845100D17F4D00CE7B4800CC784500CA744100C7703C00C56D
      3900C1602B00D2794800CABBB20055585B0042424200CFCFCF003A3A3A000000
      00FF000000FF36363600ABABAB0049494900D0D0D00039393900000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BBBCBC00FBD8BE00EEB89300EEBA
      9800ECB79400EAB49100E8B18E00E6AE8B00E5AB8900E3A88500E1A58100DFA3
      7F00DC997500EBAE8E00D3C9C40054565800232323003B3B3B00000000FF0000
      00FF000000FF34343400000000FF9A9A9A001C1C1C00DCDCDC00000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000093949400C1C2C200C2C1C000C1C0
      BF00C1C0BF00C0BFBE00C0BFBF00C0BFBF00C0BFBF00BFBFBE00C0BFBE00C0BF
      BE00C1C0BF00C9C7C7008E8E8E009E9E9E004C4C4C00000000FF000000FF0000
      00FF000000FFC7C7C700393939003D3D3D00D7D7D700000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D1D1D8009495AA009394A8009394
      A8009394A8009192A5009394A8009495AA009496AB009496AB009394A7009496
      AB009496AB009496AB009496AB00D1D1D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000ECE0D600000000009A9CB3006474EC006373EA006879
      FA006779FA006972B0006779FA006879FA00687AFE00697BFF006A73B2006474
      EB00697BFF00697BFF00697BFF009A9CB3000000000000000000000000000000
      0000000000000000000000000000F2F0D300E8E38D00E6E07F00E4DF7600E8E2
      8700F2EFBF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D3B59900AD733D00EBDED2009A9CB3006474EB00697BFF00687A
      FE006879FA00717AC1006779FA006779FA006879FA00687AFE006A73B200697B
      FF00697BFF00697BFF00697BFF009A9CB3000000000000000000F4F7D300E2EC
      8400E2EA8F0000000000E8E4A000E6E39A000000000000000000000000000000
      0000F0EDB600E7E3870000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DAF0FB004ABBF30043BBF60042BA
      F50042B8F20042B8F10041B5EE0040AFE60040AFE70040B1EA0040B2EB0040B2
      EB0078929300AD733D00A29C8D00000000009598AA006266A200767BD0006269
      A000656EAA006971A800656EA800626AA100626AA100656EA8005D638C00666E
      AB0062699E00666EAB00666EAB009799AC000000000000000000000000000000
      000000000000EDEAB600EAE8AD00000000000000000000000000000000000000
      000000000000F6F4D600EAE69200000000000000000000000000000000000000
      00007E7E7E000000000000000000000000000000000000000000000000007E7E
      7E0000000000000000000000000000000000BEE6FA0043BBF60043BBF60043BB
      F60042BAF50042B8F2004CABD7006B848C0078766C00757B76005D91A8007A8C
      8B00AC723D007D908B0075C7EF00000000009898AC005656D8005555D400606B
      B700697BFF00687AFE006879F9005A67C6005E6BD5006779FA006879FA00687A
      FE00606BB7006879FA00697BFF009A9CB3000000000000000000000000000000
      000000000000E5E17F0000000000000000000000000000000000000000000000
      00000000000000000000EBE79A00F8F7E1000000000000000000000000008787
      8700000000000000000000000000000000000000000000000000000000000000
      00007F7F7F00000000000000000000000000BEE6FA0043BBF60043BBF60043BB
      F60043BBF60052A5CA007B786E0092B5C3009ACDE60098C5DA0085949600846C
      55007C8A860040B2EB0075C7EF00000000009898AC005656D8005555D400606B
      B700697BFF00697BFF006373E9005A67C7005E6BD5006779FA006779FA006879
      FA005D67B2006879FA006475EE00979AAE000000000000000000000000000000
      0000F6F5D700EEEBAA0000000000000000000000000000000000000000000000
      00000000000000000000F6F5D700EEEBAA000000000000000000878787000000
      0000767676000000000000000000000000000000000000000000000000007E7E
      7E00000000007F7F7F000000000000000000BEE6FA0043BBF60043BBF60043BB
      F60045B9F3007B776E009BCBE1009CD5F2009BD1EC009BD1EB009BD1EB008695
      98005E91A70040B2EB0075C7EF00000000009598AA00676BAC00676AAC00676D
      A5006D76B800646A9B006D76B8006870AE007C86D7006B75B400606388006566
      86005F5F79006465860066668700918F9700F1F6C700ECF2AC00ECF2AC00F1F6
      C600F2F1C400E6E07F00000000000000000000000000A7F0D90063E8BF000000
      00000000000000000000EFEDB200E8E389000000000087878700000000007676
      7600000000007E7E7E00000000007F7F7F000000000000000000000000000000
      00007E7E7E00000000007F7F7F0000000000BEE6FA0043BBF60043BBF60043BB
      F60051A8D200818D8C009FD9F6009ED8F5009CD4F0009BD1EB009BD1EB0098C5
      DC00767A750040B1EA0075C7EF00000000009A9CB3006373E900697BFF00697B
      FF00697BFF006A73B200697BFF00697BFF00687AFE006879FA007F83A4009D99
      9600959290009794910099989400DBDBDB00F7F9E200F4F7D400F4F7D400F7F9
      E200F6F4D400EFECAF0000000000000000000000000000000000B0F2DC0098EE
      D3000000000000000000F6F5DC00EEEAA7008787870000000000767676000000
      000000000000000000007E7E7E00000000007F7F7F0000000000000000000000
      0000000000007E7E7E00000000007F7F7F00BEE6FA0043BBF60043BBF60043BB
      F60054A3C800859699009FD9F6009FD9F6009ED8F5009BD2ED009BD1EB009BCE
      E60079756D0040AFE70075C6EE00000000009A9CB3006677F500697BFF00697B
      FF00697BFF006A73B200697BFF00697BFF00697BFF00687AFE007E83A4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7E280000000000000000000000000000000000000000000B4F1
      DF009AEFD40000000000E8E59D00F6F5DC007E7E7E00000000007F7F7F000000
      00000000000000000000000000007E7E7E00000000007F7F7F00000000000000
      000000000000878787000000000076767600BEE6FA0043BBF60043BBF60043BB
      F60049B3E5007F7D74009ED8F5009FD9F6009FD9F6009ED7F3009BD1EC0091B4
      C3006C82880040AFE60074C4EA00000000009899AD006973B8006971B7005E65
      93006973B800656EAF006973B8006269A5005E5C6C005F5D6D00807F85000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EEEBAD00F2EFBE00000000000000000000000000000000000000
      0000DCF5ED00F3F2DD00E3E0890000000000000000007E7E7E00000000007F7F
      7F00000000000000000000000000000000007E7E7E00000000007F7F7F000000
      000087878700000000007676760000000000BEE6FA0043BBF60043BBF60043BB
      F60043BBF6006B8A93008BA0A4009ED8F5009FD9F6009FD9F6009AC9DE007B77
      6F004AA2CC0040AFE60074C4EA00000000009A9CB300697BFF006879FA00606B
      B700697BFF00697BFF00687AFC005C68CC00B6B4B400C4C3C300000000000000
      0000000000000000000000000000000000000000000000000000F4F7D300E3ED
      8400E6EF900000000000EAE69400EEEBAC000000000000000000000000000000
      0000EFEDC500E1DC7C00000000000000000000000000000000007E7E7E000000
      00007F7F7F000000000000000000000000000000000000000000000000008787
      870000000000767676000000000000000000BEE6FA0043BBF60043BBF60043BB
      F60043BBF60045B9F1006B8A92007F7D740085979A00828E8D007C776D00519D
      C00040AFE70040AFE60074C4EA00000000009A9CB2006576F1006575ED005E68
      AF006576F1006576F1006475ED005663BC00DEDEDE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F2F1C400E6E38300E6E07800E1DC6800E6E3
      8300EEEBAD000000000000000000000000000000000000000000000000007E7E
      7E00000000000000000000000000000000000000000000000000000000000000
      000076767600000000000000000000000000BEE6FA0043BBF60043BBF60043BB
      F60043BBF60043BBF60043BBF6004AB1E40055A2C60051A7D00044B9F20041B4
      ED0040B1EA0040AFE70074C4EA0000000000DEDEE300ABACB900ABACB900ABAD
      B900ABACB900ABACB900ABACB900B6B6C50000000000000000009BA0BE00757D
      AB00757DAB00747BAA00777DAB00C0C2D1000000000000000000000000000000
      000000000000000000000000000000000000F8F7E300EBE89F00E7E38500F5F3
      CF00000000000000000000000000000000000000000000000000000000000000
      00007E7E7E000000000000000000000000000000000000000000000000007676
      760000000000000000000000000000000000BAD8E800368FC100368FC100368F
      C100368FC100368FC100368FC100368FC100368FC100368FC100368FC100368F
      C100368FC100368EC0006FABCD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007C82B300697B
      FF00697BFF00697BFF006373EA00989AB1000000000000000000000000000000
      000000000000000000000000000000000000E8E38600F4F2CC00F4F2CC00E6E1
      7C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BAD8E700368FC100368FC100368F
      C100368FC100368FC100368FC100368FC100368FC100368FC100368FC100368F
      C100368FC100368FC10090C0DB00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007C82B300697B
      FF00697BFF006474EC006373EA00999BB2000000000000000000000000000000
      000000000000000000000000000000000000F5F4D200EBE79A00EBE79A00F0EE
      BA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BCD7E600368EC000368FC100368F
      C100368FC100368FC10061A5CD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C3C5D5009496
      AB009496AB009496AB009699AE00E2E3EC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BCD7E600BAD8E700BAD8
      E800BAD8E800C0DBEA0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFE100006D290000
      FF0D00006F730000FE0D000010670000FE610000304C0000FE7F000074800000
      8661000031200000B00D000072480000B00D00003C8100008661000019030000
      FE7F000072070000FE610000240F0000FE0D0000091F0000FF0D0000183F0000
      FFE100003A3F0000FFFF0000787F00000000FFFFFFFFFFFD0000FE07FFFFFFF8
      0000C4F3FFFF00010000F9F9F7EF00010000FBFCE7E700010000F3FCC7E30001
      0000039C88F10001000003CC1C780001001FFBE41E380001001FF9F18F110001
      003FC4F3C7E30001007FFE07E7E7000100C0FF0FF7EF0001FFC0FF0FFFFF0001
      FFC0FF0FFFFF01FFFFC0FFFFFFFF83FF00000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 656
    Top = 384
    object actCompilersDeselectAll: TAction
      Category = 'Compilers'
      Caption = '&UnCheck All'
      OnExecute = actCompilersDeselectAllExecute
    end
    object actDeleteTemplate: TAction
      Category = 'Templates'
      Caption = 'Delete Template'
      OnExecute = actDeleteTemplateExecute
    end
    object actDuplicateTemplate: TAction
      Category = 'Templates'
      Caption = 'Duplicate Template'
      OnExecute = actDuplicateTemplateExecute
    end
    object actAddBuildItem: TAction
      Category = 'Build'
      Caption = 'Add Build Item'
      OnExecute = actAddBuildItemExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open..'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &As..'
      ShortCut = 24659
      OnExecute = actFileSaveAsExecute
    end
    object actFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = actFileNewExecute
    end
    object actDeleteBuildItem: TAction
      Category = 'Build'
      Caption = 'Delect Build Item'
      OnExecute = actDeleteBuildItemExecute
    end
    object actAddRuntimeItem: TAction
      Category = 'Runtime'
      Caption = 'Add Runtime Item'
      OnExecute = actAddRuntimeItemExecute
    end
    object actDeleteRuntimeItem: TAction
      Category = 'Runtime'
      Caption = 'Delete Runtime Item'
      OnExecute = actDeleteRuntimeItemExecute
    end
    object actAddDesignItem: TAction
      Category = 'Design'
      Caption = 'Add Design Item'
      OnExecute = actAddDesignItemExecute
    end
    object actDeleteDesignItem: TAction
      Category = 'Design'
      Caption = 'Delete Design Item'
      OnExecute = actDeleteDesignItemExecute
    end
    object actAddSourceItem: TAction
      Category = 'Source'
      Caption = 'Add Source Item'
      OnExecute = actAddSourceItemExecute
    end
    object actAddFileItem: TAction
      Category = 'File'
      Caption = 'Add File Item'
      OnExecute = actAddFileItemExecute
    end
    object actDeleteSourceItem: TAction
      Category = 'Source'
      Caption = 'Delete Source Item'
      OnExecute = actDeleteSourceItemExecute
    end
    object actDeleteFileItem: TAction
      Category = 'File'
      Caption = 'Delete File Item'
      OnExecute = actDeleteFileItemExecute
    end
    object actAddLibItem: TAction
      Category = 'Lib'
      Caption = 'Add Lib Item'
      OnExecute = actAddLibItemExecute
    end
    object actDeleteLibItem: TAction
      Category = 'Lib'
      Caption = 'Delete Lib Item'
      OnExecute = actDeleteLibItemExecute
    end
    object actAddSearchPath: TAction
      Category = 'SearchPath'
      Caption = 'Add Search Path'
      OnExecute = actAddSearchPathExecute
    end
    object actDeleteSearchPath: TAction
      Category = 'SearchPath'
      Caption = 'Delete Search Path'
      OnExecute = actDeleteSearchPathExecute
    end
    object actAddDependency: TAction
      Category = 'Dependency'
      Caption = 'Add Dependency'
      OnExecute = actAddDependencyExecute
    end
    object actDeleteDependency: TAction
      Category = 'Dependency'
      Caption = 'Delete Dependency'
      OnExecute = actDeleteDependencyExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = actFileExitExecute
    end
    object actCompilersSelectAll: TAction
      Category = 'Compilers'
      Caption = 'Check &All'
      OnExecute = actCompilersSelectAllExecute
    end
    object actPlatformsSelectAll: TAction
      Category = 'Platforms'
      Caption = 'Check &All'
      OnExecute = actPlatformsSelectAllExecute
    end
    object actPlatformsDeselectAll: TAction
      Category = 'Platforms'
      Caption = '&UnCheck All'
      OnExecute = actPlatformsDeselectAllExecute
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 
      'All (*.svg, *.png)|*.svg; *.png|Scalable Vector Graphics (*.svg)' +
      '|*.svg|Portable Network Graphics (*.png)|*.png'
    Left = 740
    Top = 226
  end
  object pmCompilers: TPopupMenu
    Left = 588
    Top = 442
    object SelectAll1: TMenuItem
      Action = actCompilersSelectAll
    end
    object DeselectAll1: TMenuItem
      Action = actCompilersDeselectAll
    end
  end
  object pmPlatforms: TPopupMenu
    Left = 660
    Top = 442
    object SelectAll2: TMenuItem
      Action = actPlatformsSelectAll
    end
    object DeselectAll2: TMenuItem
      Action = actPlatformsDeselectAll
    end
  end
end
