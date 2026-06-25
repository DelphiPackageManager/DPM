object DSpecCreatorForm: TDSpecCreatorForm
  Left = 0
  Top = 0
  Caption = '.dspec Creator'
  ClientHeight = 587
  ClientWidth = 886
  Color = clBtnFace
  Constraints.MinHeight = 589
  Constraints.MinWidth = 880
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 886
    Height = 568
    ActivePage = tsInfo
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    ExplicitWidth = 871
    ExplicitHeight = 558
    object tsInfo: TTabSheet
      Caption = 'Package Info'
      DesignSize = (
        878
        538)
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
      object lblPackageVariables: TLabel
        Left = 488
        Top = 165
        Width = 93
        Height = 15
        Caption = 'Package Variables'
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
        Top = 125
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
      object edtRepositoryCommit: TEdit
        Left = 89
        Top = 345
        Width = 376
        Height = 23
        TabOrder = 8
        OnChange = edtRepositoryCommitChange
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
        TabOrder = 9
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
        TabOrder = 10
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
        TabOrder = 11
        OnChange = edtReadmeChange
      end
      object chkIsCommercial: TCheckBox
        Left = 89
        Top = 502
        Width = 180
        Height = 17
        Caption = 'Commercial package'
        TabOrder = 13
        OnClick = chkIsCommercialClick
      end
      object chkIsTrial: TCheckBox
        Left = 285
        Top = 502
        Width = 180
        Height = 17
        Caption = 'Trial package'
        TabOrder = 14
        OnClick = chkIsTrialClick
      end
      object PackageVariablesList: TValueListEditor
        Left = 488
        Top = 184
        Width = 387
        Height = 351
        Anchors = [akLeft, akTop, akRight, akBottom]
        Constraints.MinHeight = 313
        DoubleBuffered = True
        KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
        ParentDoubleBuffered = False
        TabOrder = 12
        TitleCaptions.Strings = (
          'Variable Name'
          'Value')
        OnStringsChange = PackageVariablesListStringsChange
        ExplicitWidth = 372
        ExplicitHeight = 341
        ColWidths = (
          150
          231)
      end
    end
    object tsPlatforms: TTabSheet
      Caption = 'TargetPlatforms'
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
      object Label9: TLabel
        Left = 256
        Top = 315
        Width = 176
        Height = 15
        Caption = 'TargetPlatform Variable Overrides'
      end
      object clbCompilers: TCheckListBox
        Left = 72
        Top = 28
        Width = 161
        Height = 469
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
          'WinARM64EC'
          'MacOS32'
          'MacOS64'
          'MacOSARM64'
          'Android'
          'Android64'
          'iOS32'
          'iOS64'
          'iOSSimulator'
          'Linux64')
        ParentDoubleBuffered = False
        PopupMenu = pmPlatforms
        TabOrder = 2
        OnClickCheck = clbPlatformsClickCheck
      end
      object VariablesList: TValueListEditor
        Left = 256
        Top = 336
        Width = 585
        Height = 153
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
          429)
      end
    end
    object tsTemplates: TTabSheet
      Caption = 'Templates'
      ImageIndex = 2
      object Splitter1: TSplitter
        Left = 330
        Top = 0
        Width = 4
        Height = 538
        ResizeStyle = rsUpdate
        ExplicitHeight = 528
      end
      object CardPanel: TCardPanel
        Left = 334
        Top = 0
        Width = 544
        Height = 538
        Align = alClient
        ActiveCard = crdCopyLocal
        Caption = 'CardPanel'
        TabOrder = 0
        ExplicitWidth = 529
        ExplicitHeight = 528
        object crdSource: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Source'
          CardIndex = 0
          TabOrder = 0
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
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
          object lblFileEntryCopyToBin: TLabel
            Left = 16
            Top = 336
            Width = 142
            Height = 15
            Caption = 'Copy to bpl\{platform} for:'
          end
          object edtFileEntrySource: TEdit
            Left = 16
            Top = 61
            Width = 380
            Height = 23
            CustomHint = BalloonHint1
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtFileEntrySourceChange
            ExplicitWidth = 365
          end
          object chkFileEntryCopyToLib: TCheckBox
            Left = 16
            Top = 89
            Width = 297
            Height = 17
            Hint = 
              'Use this to copy form and resource files to the lib\{platform} f' +
              'older'
            Caption = 'Copy matched files to lib\{platform} on install'
            TabOrder = 6
            OnClick = chkFileEntryCopyToLibClick
          end
          object edtFileEntryDest: TEdit
            Left = 16
            Top = 134
            Width = 380
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            TextHint = 'Leave blank to use same relative path as source'
            OnChange = edtFileEntryDestChange
            ExplicitWidth = 365
          end
          object lbFileEntryExclude: TListBox
            Left = 16
            Top = 182
            Width = 380
            Height = 97
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 15
            TabOrder = 2
            OnDblClick = lbFileEntryExcludeDblClick
            ExplicitWidth = 365
          end
          object btnAddExclude: TButton
            Left = 16
            Top = 293
            Width = 88
            Height = 25
            Caption = 'Add Exclude'
            TabOrder = 4
            OnClick = btnAddExcludeClick
          end
          object btnEditExclude: TButton
            Left = 147
            Top = 293
            Width = 88
            Height = 25
            Caption = 'Edit Exclude'
            TabOrder = 5
            OnClick = btnEditExcludeClick
          end
          object btnDeleteExclude: TButton
            Left = 293
            Top = 293
            Width = 88
            Height = 25
            Caption = 'Delete Exclude'
            TabOrder = 3
            OnClick = btnDeleteExcludeClick
          end
          object cboFileEntryCopyToBin: TComboBox
            Left = 16
            Top = 354
            Width = 250
            Height = 23
            Style = csDropDownList
            TabOrder = 7
            OnChange = cboFileEntryCopyToBinChange
          end
        end
        object crdBuild: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Build Item'
          CardIndex = 1
          TabOrder = 1
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblBuild: TLabel
            Left = 8
            Top = 8
            Width = 27
            Height = 15
            Caption = 'Build'
          end
          object lblProject: TLabel
            Left = 16
            Top = 40
            Width = 82
            Height = 15
            Caption = 'Project (.dproj):'
          end
          object lblBuildDefines: TLabel
            Left = 16
            Top = 92
            Width = 162
            Height = 15
            Caption = 'Defines (semicolon separated):'
          end
          object lblBuildPlatforms: TLabel
            Left = 16
            Top = 144
            Width = 108
            Height = 15
            Caption = 'Platforms (override):'
          end
          object lblBuildReferences: TLabel
            Left = 271
            Top = 144
            Width = 60
            Height = 15
            Caption = 'References:'
          end
          object edtProject: TEdit
            Left = 16
            Top = 61
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtProjectChange
            ExplicitWidth = 495
          end
          object edtBuildDefines: TEdit
            Left = 16
            Top = 113
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            OnChange = edtBuildDefinesChange
            ExplicitWidth = 495
          end
          object clbBuildPlatforms: TCheckListBox
            Left = 16
            Top = 165
            Width = 240
            Height = 160
            ItemHeight = 17
            Items.Strings = (
              'Win32'
              'Win64'
              'WinARM64EC'
              'MacOS32'
              'MacOS64'
              'MacOSARM64'
              'Android'
              'Android64'
              'iOS32'
              'iOS64'
              'iOSSimulator'
              'Linux64')
            TabOrder = 2
            OnClickCheck = clbBuildPlatformsClickCheck
          end
          object lbBuildReferences: TListBox
            Left = 271
            Top = 165
            Width = 240
            Height = 120
            ItemHeight = 15
            TabOrder = 3
          end
          object btnAddBuildRef: TButton
            Left = 271
            Top = 293
            Width = 75
            Height = 25
            Caption = 'Add'
            TabOrder = 4
            OnClick = btnAddBuildRefClick
          end
          object btnDeleteBuildRef: TButton
            Left = 360
            Top = 293
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 5
            OnClick = btnDeleteBuildRefClick
          end
        end
        object crdDesign: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Design Item'
          CardIndex = 2
          TabOrder = 2
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblDesign: TLabel
            Left = 8
            Top = 8
            Width = 36
            Height = 15
            Caption = 'Design'
          end
          object lblDesignProject: TLabel
            Left = 16
            Top = 40
            Width = 82
            Height = 15
            Caption = 'Project (.dproj):'
          end
          object lblDesignDefines: TLabel
            Left = 16
            Top = 92
            Width = 162
            Height = 15
            Caption = 'Defines (semicolon separated):'
          end
          object lblDesignPlatforms: TLabel
            Left = 16
            Top = 144
            Width = 54
            Height = 15
            Caption = 'Platforms:'
          end
          object lblDesignReferences: TLabel
            Left = 16
            Top = 318
            Width = 212
            Height = 15
            Caption = 'References (only used for package defs):'
          end
          object lblLibPrefix: TLabel
            Left = 264
            Top = 176
            Width = 51
            Height = 15
            Caption = 'Lib Prefix:'
          end
          object lblLibSuffix: TLabel
            Left = 264
            Top = 205
            Width = 51
            Height = 15
            Caption = 'Lib Suffix:'
          end
          object lblLibVersion: TLabel
            Left = 264
            Top = 234
            Width = 60
            Height = 15
            Caption = 'Lib Version:'
          end
          object edtDesignProject: TEdit
            Left = 16
            Top = 61
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtDesignProjectChange
            ExplicitWidth = 495
          end
          object edtDesignDefines: TEdit
            Left = 16
            Top = 113
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            OnChange = edtDesignDefinesChange
            ExplicitWidth = 495
          end
          object clbDesignPlatforms: TCheckListBox
            Left = 16
            Top = 165
            Width = 240
            Height = 140
            ItemHeight = 17
            Items.Strings = (
              'Win32'
              'Win64'
              'WinARM64EC'
              'MacOS32'
              'MacOS64'
              'MacOSARM64'
              'Android'
              'Android64'
              'iOS32'
              'iOS64'
              'iOSSimulator'
              'Linux64')
            TabOrder = 2
            OnClickCheck = clbDesignPlatformsClickCheck
          end
          object lbDesignReferences: TListBox
            Left = 16
            Top = 339
            Width = 240
            Height = 100
            ItemHeight = 15
            TabOrder = 3
          end
          object btnAddDesignRef: TButton
            Left = 16
            Top = 445
            Width = 75
            Height = 25
            Caption = 'Add'
            TabOrder = 4
            OnClick = btnAddDesignRefClick
          end
          object btnDeleteDesignRef: TButton
            Left = 105
            Top = 445
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 5
            OnClick = btnDeleteDesignRefClick
          end
          object edtLibPrefix: TEdit
            Left = 358
            Top = 173
            Width = 150
            Height = 23
            TabOrder = 6
            OnChange = edtLibPrefixChange
          end
          object edtLibSuffix: TEdit
            Left = 358
            Top = 202
            Width = 150
            Height = 23
            TabOrder = 7
            OnChange = edtLibSuffixChange
          end
          object edtLibVersion: TEdit
            Left = 358
            Top = 231
            Width = 150
            Height = 23
            TabOrder = 8
            OnChange = edtLibVersionChange
          end
        end
        object crdDependency: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Dependency'
          CardIndex = 3
          TabOrder = 3
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
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
          object edtDependencyId: TEdit
            Left = 16
            Top = 61
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = edtDependencyIdChange
            ExplicitWidth = 405
          end
          object edtDependencyVersion: TEdit
            Left = 16
            Top = 116
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = edtDependencyVersionChange
            ExplicitWidth = 405
          end
        end
        object crdTemplate: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Templates'
          CardIndex = 4
          TabOrder = 4
          ExplicitWidth = 527
          ExplicitHeight = 526
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
          Width = 542
          Height = 536
          Caption = 'Build Items Heading'
          CardIndex = 5
          TabOrder = 5
          ExplicitWidth = 527
          ExplicitHeight = 526
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
        object crdSourceHeading: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Source Items Heading'
          CardIndex = 6
          TabOrder = 6
          ExplicitWidth = 527
          ExplicitHeight = 526
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
          Width = 542
          Height = 536
          Caption = 'Dependencies '
          CardIndex = 7
          TabOrder = 7
          ExplicitWidth = 527
          ExplicitHeight = 526
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
        object crdDesignHeading: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Design Heading'
          CardIndex = 8
          TabOrder = 8
          ExplicitWidth = 527
          ExplicitHeight = 526
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
        object crdPackageDefsHeading: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Package Definitions'
          CardIndex = 9
          TabOrder = 9
          ExplicitWidth = 527
          ExplicitHeight = 526
          object lblPackageDefsHeading: TLabel
            Left = 24
            Top = 24
            Width = 104
            Height = 15
            Caption = 'Package Definitions'
          end
          object lblPackageDefsDescription: TLabel
            Left = 24
            Top = 48
            Width = 497
            Height = 292
            AutoSize = False
            Caption = 
              'Package Definitions define how to generate package projects for ' +
              'your source code. '#13#10#13#10'Use this if your project does not already ' +
              'have delphi packages. '#13#10#13#10'Without packages, dpm cannot compile t' +
              'he library, so users projects will refrence the source, resultin' +
              'g in longer build times.'
            Enabled = False
            WordWrap = True
          end
        end
        object crdPackageDef: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Package Definition'
          CardIndex = 10
          TabOrder = 10
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblPackageDef: TLabel
            Left = 8
            Top = 8
            Width = 99
            Height = 15
            Caption = 'Package Definition'
          end
          object lblPackageDefProject: TLabel
            Left = 16
            Top = 40
            Width = 82
            Height = 15
            Caption = 'Project (.dproj):'
          end
          object lblPackageDefKind: TLabel
            Left = 16
            Top = 92
            Width = 27
            Height = 15
            Caption = 'Kind:'
          end
          object lblPackageDefPlatforms: TLabel
            Left = 16
            Top = 150
            Width = 196
            Height = 15
            Caption = 'Platforms (overrides target platform):'
          end
          object lblPackageDefFiles: TLabel
            Left = 271
            Top = 150
            Width = 26
            Height = 15
            Caption = 'Files:'
          end
          object lblPackageDefExclude: TLabel
            Left = 16
            Top = 331
            Width = 43
            Height = 15
            Caption = 'Exclude:'
          end
          object lblPackageDefRequires: TLabel
            Left = 271
            Top = 331
            Width = 48
            Height = 15
            Caption = 'Requires:'
          end
          object edtPackageDefProject: TEdit
            Left = 16
            Top = 61
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = edtPackageDefProjectChange
            ExplicitWidth = 495
          end
          object cboPackageDefKind: TComboBox
            Left = 16
            Top = 113
            Width = 200
            Height = 23
            TabOrder = 1
            OnChange = cboPackageDefKindChange
            Items.Strings = (
              ''
              'runtime'
              'design')
          end
          object clbPackageDefPlatforms: TCheckListBox
            Left = 16
            Top = 171
            Width = 240
            Height = 140
            ItemHeight = 17
            Items.Strings = (
              'Win32'
              'Win64'
              'WinARM64EC'
              'MacOS32'
              'MacOS64'
              'MacOSARM64'
              'Android'
              'Android64'
              'iOS32'
              'iOS64'
              'iOSSimulator'
              'Linux64')
            TabOrder = 2
            OnClickCheck = clbPackageDefPlatformsClickCheck
          end
          object lbPackageDefFiles: TListBox
            Left = 271
            Top = 171
            Width = 255
            Height = 110
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 15
            TabOrder = 3
            ExplicitWidth = 240
          end
          object btnAddPackageDefFile: TButton
            Left = 271
            Top = 287
            Width = 75
            Height = 25
            Caption = 'Add'
            TabOrder = 4
            OnClick = btnAddPackageDefFileClick
          end
          object btnDeletePackageDefFile: TButton
            Left = 360
            Top = 287
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 5
            OnClick = btnDeletePackageDefFileClick
          end
          object lbPackageDefExclude: TListBox
            Left = 16
            Top = 352
            Width = 240
            Height = 110
            ItemHeight = 15
            TabOrder = 6
          end
          object btnAddPackageDefExclude: TButton
            Left = 16
            Top = 468
            Width = 75
            Height = 25
            Caption = 'Add'
            TabOrder = 7
            OnClick = btnAddPackageDefExcludeClick
          end
          object btnDeletePackageDefExclude: TButton
            Left = 105
            Top = 468
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 8
            OnClick = btnDeletePackageDefExcludeClick
          end
          object lbPackageDefRequires: TListBox
            Left = 271
            Top = 352
            Width = 255
            Height = 110
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 15
            TabOrder = 9
            ExplicitWidth = 240
          end
          object btnAddPackageDefRequire: TButton
            Left = 271
            Top = 468
            Width = 75
            Height = 25
            Caption = 'Add'
            TabOrder = 10
            OnClick = btnAddPackageDefRequireClick
          end
          object btnDeletePackageDefRequire: TButton
            Left = 360
            Top = 468
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 11
            OnClick = btnDeletePackageDefRequireClick
          end
        end
        object crdEnvironmentVariables: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Environment Variables'
          CardIndex = 11
          TabOrder = 11
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblEnvironmentVariablesHeading: TLabel
            Left = 8
            Top = 8
            Width = 117
            Height = 15
            Caption = 'Environment Variables'
          end
          object lblEnvironmentVariablesDescription: TLabel
            Left = 8
            Top = 32
            Width = 505
            Height = 60
            AutoSize = False
            Caption = 
              'Environment variables set in the IDE process while the package d' +
              'esign-time components are loaded, and cleared when unloaded. PAT' +
              'H is appended (semicolon separated). Use $packageDir$ to referen' +
              'ce the package cache folder.'
            Enabled = False
            WordWrap = True
          end
          object envVariablesList: TValueListEditor
            Left = 8
            Top = 98
            Width = 526
            Height = 429
            Anchors = [akLeft, akTop, akRight, akBottom]
            DoubleBuffered = True
            KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
            ParentDoubleBuffered = False
            TabOrder = 0
            TitleCaptions.Strings = (
              'Variable Name'
              'Value')
            OnStringsChange = envVariablesListStringsChange
            ExplicitWidth = 511
            ExplicitHeight = 419
            ColWidths = (
              150
              355)
          end
        end
        object crdCopyLocal: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Copy Local'
          CardIndex = 12
          TabOrder = 12
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblCopyLocal: TLabel
            Left = 8
            Top = 8
            Width = 59
            Height = 15
            Caption = 'Copy Local'
          end
          object lblCopyLocalSrc: TLabel
            Left = 16
            Top = 40
            Width = 19
            Height = 15
            Caption = 'Src:'
          end
          object lblCopyLocalPlatforms: TLabel
            Left = 16
            Top = 92
            Width = 54
            Height = 15
            Caption = 'Platforms:'
          end
          object edtCopyLocalSrc: TEdit
            Left = 16
            Top = 61
            Width = 510
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            TextHint = 'e.g. bin\$platform$\*.dll'
            OnChange = edtCopyLocalSrcChange
            ExplicitWidth = 495
          end
          object clbCopyLocalPlatforms: TCheckListBox
            Left = 16
            Top = 113
            Width = 240
            Height = 160
            ItemHeight = 17
            Items.Strings = (
              'Win32'
              'Win64'
              'WinARM64EC'
              'MacOS32'
              'MacOS64'
              'MacOSARM64'
              'Android'
              'Android64'
              'iOS32'
              'iOS64'
              'iOSSimulator'
              'Linux64')
            TabOrder = 1
            OnClickCheck = clbCopyLocalPlatformsClickCheck
          end
        end
        object crdCopyLocalHeading: TCard
          Left = 1
          Top = 1
          Width = 542
          Height = 536
          Caption = 'Copy Local'
          CardIndex = 13
          TabOrder = 13
          ExplicitWidth = 527
          ExplicitHeight = 526
          DesignSize = (
            542
            536)
          object lblCopyLocalHeading: TLabel
            Left = 8
            Top = 8
            Width = 59
            Height = 15
            Caption = 'Copy Local'
          end
          object Label12: TLabel
            Left = 5
            Top = 29
            Width = 537
            Height = 70
            Anchors = [akLeft, akTop, akRight, akBottom]
            AutoSize = False
            Caption = 
              'Copy Local items definre how files in the package should be copi' +
              'ed to the output folder after build. Use this for runtime bpl'#39's,' +
              ' dll'#39's etc'
            Enabled = False
            WordWrap = True
            ExplicitWidth = 522
            ExplicitHeight = 60
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 330
        Height = 538
        Align = alLeft
        Caption = 'Panel2'
        ShowCaption = False
        TabOrder = 1
        ExplicitHeight = 528
        DesignSize = (
          330
          538)
        object lblTemplateView: TLabel
          Left = 3
          Top = 3
          Width = 77
          Height = 15
          Caption = 'Template View'
        end
        object Label3: TLabel
          Left = 3
          Top = 479
          Width = 161
          Height = 15
          Anchors = [akLeft, akBottom]
          Caption = 'Right click to add/delete items'
          Enabled = False
          ExplicitTop = 469
        end
        object tvTemplates: TTreeView
          Left = 3
          Top = 24
          Width = 319
          Height = 449
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
          ExplicitHeight = 439
        end
        object btnAddTemplate: TButton
          Left = 3
          Top = 500
          Width = 86
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Add Template'
          TabOrder = 1
          OnClick = btnAddTemplateClick
          ExplicitTop = 490
        end
        object btnDeleteTemplate: TButton
          Left = 95
          Top = 500
          Width = 98
          Height = 25
          Action = actDeleteTemplate
          Anchors = [akLeft, akBottom]
          TabOrder = 2
          ExplicitTop = 490
        end
        object btnDuplicateTemplate: TButton
          Left = 200
          Top = 500
          Width = 122
          Height = 25
          Action = actDuplicateTemplate
          Anchors = [akLeft, akBottom]
          TabOrder = 3
          ExplicitTop = 490
        end
      end
    end
    object tsSigning: TTabSheet
      Caption = 'Signing'
      ImageIndex = 5
      object pnlProviders: TPanel
        Left = 0
        Top = 33
        Width = 878
        Height = 505
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 863
        ExplicitHeight = 495
        object Label8: TLabel
          Left = 16
          Top = 6
          Width = 44
          Height = 15
          Caption = 'Provider'
        end
        object lblTimestampUrl: TLabel
          Left = 248
          Top = 6
          Width = 84
          Height = 15
          Caption = 'Timestamp URL'
        end
        object lblDigest: TLabel
          Left = 568
          Top = 6
          Width = 33
          Height = 15
          Caption = 'Digest'
        end
        object cboSigningProvider: TComboBox
          Left = 16
          Top = 32
          Width = 209
          Height = 23
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Windows Certificate Store'
          OnChange = cboSigningProviderChange
          Items.Strings = (
            'Windows Certificate Store'
            'PFX file'
            'Signotaur'
            'Azure Key Vault')
        end
        object edtTimestampUrl: TEdit
          Left = 248
          Top = 32
          Width = 305
          Height = 23
          TabOrder = 1
          Text = 'http://timestamp.digicert.com'
        end
        object cboDigest: TComboBox
          Left = 568
          Top = 32
          Width = 137
          Height = 23
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 2
          Text = 'Auto'
          Items.Strings = (
            'Auto'
            'SHA256'
            'SHA384'
            'SHA512')
        end
        object PageControl1: TPageControl
          Left = 16
          Top = 70
          Width = 841
          Height = 419
          ActivePage = TabSheet3
          TabOrder = 3
          object TabSheet1: TTabSheet
            Caption = 'Windows Certificate Store'
            object lblCertThumbprint: TLabel
              Left = 16
              Top = 16
              Width = 121
              Height = 15
              Caption = 'Certificate Thumbprint'
            end
            object lblStoreLocation: TLabel
              Left = 16
              Top = 70
              Width = 76
              Height = 15
              Caption = 'Store Location'
            end
            object edtCertThumbprint: TEdit
              Left = 16
              Top = 34
              Width = 500
              Height = 23
              TabOrder = 0
            end
            object cboStoreLocation: TComboBox
              Left = 16
              Top = 88
              Width = 200
              Height = 23
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 1
              Text = 'Current User'
              Items.Strings = (
                'Current User'
                'Local Machine')
            end
          end
          object TabSheet2: TTabSheet
            Caption = 'PFX file'
            ImageIndex = 1
            object lblPfxFile: TLabel
              Left = 16
              Top = 16
              Width = 41
              Height = 15
              Caption = 'PFX File'
            end
            object lblPfxPassword: TLabel
              Left = 16
              Top = 70
              Width = 50
              Height = 15
              Caption = 'Password'
            end
            object edtPfxFile: TEdit
              Left = 16
              Top = 34
              Width = 460
              Height = 23
              TabOrder = 0
            end
            object btnBrowsePfx: TButton
              Left = 482
              Top = 33
              Width = 31
              Height = 25
              Caption = '...'
              TabOrder = 1
              OnClick = btnBrowsePfxClick
            end
            object edtPfxPassword: TEdit
              Left = 16
              Top = 88
              Width = 300
              Height = 23
              PasswordChar = '*'
              TabOrder = 2
            end
          end
          object TabSheet3: TTabSheet
            Caption = 'Signotaur'
            ImageIndex = 2
            object lblSignotaurEndpoint: TLabel
              Left = 16
              Top = 32
              Width = 48
              Height = 15
              Caption = 'Endpoint'
            end
            object lblSignotaurApiKey: TLabel
              Left = 16
              Top = 86
              Width = 40
              Height = 15
              Caption = 'API Key'
            end
            object lblSignotaurThumbprint: TLabel
              Left = 16
              Top = 140
              Width = 64
              Height = 15
              Caption = 'Thumbprint'
            end
            object lblSignotaurSubject: TLabel
              Left = 16
              Top = 194
              Width = 39
              Height = 15
              Caption = 'Subject'
            end
            object lblSignotaurLabel: TLabel
              Left = 16
              Top = 248
              Width = 28
              Height = 15
              Caption = 'Label'
            end
            object Label15: TLabel
              Left = 16
              Top = 11
              Width = 227
              Height = 15
              Caption = 'NOTE : Signotaur version 2 or later required'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clOrange
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              StyleElements = [seBorder]
            end
            object edtSignotaurEndpoint: TEdit
              Left = 16
              Top = 50
              Width = 500
              Height = 23
              TabOrder = 0
            end
            object edtSignotaurApiKey: TEdit
              Left = 16
              Top = 104
              Width = 300
              Height = 23
              PasswordChar = '*'
              TabOrder = 1
            end
            object edtSignotaurThumbprint: TEdit
              Left = 16
              Top = 158
              Width = 500
              Height = 23
              TabOrder = 2
            end
            object edtSignotaurSubject: TEdit
              Left = 16
              Top = 212
              Width = 500
              Height = 23
              TabOrder = 3
            end
            object edtSignotaurLabel: TEdit
              Left = 16
              Top = 266
              Width = 300
              Height = 23
              TabOrder = 4
            end
            object chkSignotaurAllowSelfSigned: TCheckBox
              Left = 16
              Top = 306
              Width = 350
              Height = 17
              Caption = 'Allow self-signed TLS certificates (dev only)'
              TabOrder = 5
            end
          end
          object TabSheet4: TTabSheet
            Caption = 'Azure Key Vault'
            ImageIndex = 3
            object lblVaultUrl: TLabel
              Left = 16
              Top = 16
              Width = 50
              Height = 15
              Caption = 'Vault URL'
            end
            object lblCertName: TLabel
              Left = 16
              Top = 70
              Width = 89
              Height = 15
              Caption = 'Certificate Name'
            end
            object lblKeyVersion: TLabel
              Left = 16
              Top = 124
              Width = 60
              Height = 15
              Caption = 'Key Version'
            end
            object lblTenantId: TLabel
              Left = 16
              Top = 178
              Width = 49
              Height = 15
              Caption = 'Tenant Id'
            end
            object lblClientId: TLabel
              Left = 16
              Top = 232
              Width = 44
              Height = 15
              Caption = 'Client Id'
            end
            object lblClientSecret: TLabel
              Left = 16
              Top = 286
              Width = 66
              Height = 15
              Caption = 'Client Secret'
            end
            object edtVaultUrl: TEdit
              Left = 16
              Top = 34
              Width = 500
              Height = 23
              TabOrder = 0
            end
            object edtCertName: TEdit
              Left = 16
              Top = 88
              Width = 300
              Height = 23
              TabOrder = 1
            end
            object edtKeyVersion: TEdit
              Left = 16
              Top = 142
              Width = 300
              Height = 23
              TabOrder = 2
            end
            object edtTenantId: TEdit
              Left = 16
              Top = 196
              Width = 400
              Height = 23
              TabOrder = 3
            end
            object edtClientId: TEdit
              Left = 16
              Top = 250
              Width = 400
              Height = 23
              TabOrder = 4
            end
            object edtClientSecret: TEdit
              Left = 16
              Top = 304
              Width = 300
              Height = 23
              PasswordChar = '*'
              TabOrder = 5
            end
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 878
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        ExplicitWidth = 863
        object chkEnableSigning: TCheckBox
          Left = 16
          Top = 8
          Width = 209
          Height = 17
          Caption = 'Enable Package Signing After Pack'
          TabOrder = 0
          OnClick = chkEnableSigningClick
        end
      end
    end
    object tsGenerate: TTabSheet
      Caption = 'Pack'
      ImageIndex = 3
      object GridPanel1: TGridPanel
        Left = 0
        Top = 0
        Width = 878
        Height = 538
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
            Control = PackLogMemo
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
        ExplicitWidth = 863
        ExplicitHeight = 528
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 876
          Height = 89
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 861
          ExplicitHeight = 88
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
            Hint = 'Generate package files'
            Caption = 'Pack'
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
          object btnCancelPack: TButton
            Left = 16
            Top = 53
            Width = 97
            Height = 25
            Hint = 'Cancel the running pack/sign (Esc)'
            Caption = 'Cancel'
            Enabled = False
            TabOrder = 2
            OnClick = btnCancelPackClick
          end
        end
        object PackLogMemo: TMemo
          Left = 1
          Top = 90
          Width = 876
          Height = 447
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          ExplicitTop = 89
          ExplicitWidth = 861
          ExplicitHeight = 438
        end
      end
    end
    object tsTest: TTabSheet
      Caption = 'Test'
      ImageIndex = 7
      object pnlTestTop: TPanel
        Left = 0
        Top = 0
        Width = 878
        Height = 200
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 863
        DesignSize = (
          878
          200)
        object lblTestCompilers: TLabel
          Left = 16
          Top = 12
          Width = 93
          Height = 15
          Caption = 'Compilers to test:'
        end
        object lblTestLog: TLabel
          Left = 16
          Top = 178
          Width = 23
          Height = 15
          Caption = 'Log:'
        end
        object lblTestHelp: TLabel
          Left = 368
          Top = 33
          Width = 496
          Height = 152
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'Testing attempts to install the package files in the DPM cache -' +
            ' equivalent to '#39'dpm cache install/remove'#39' - this will force the ' +
            'packages to be compiled. '
          Enabled = False
          WordWrap = True
          ExplicitWidth = 481
        end
        object clbTestCompilers: TCheckListBox
          Left = 16
          Top = 33
          Width = 200
          Height = 134
          ItemHeight = 17
          Style = lbOwnerDrawFixed
          TabOrder = 0
          OnClick = clbTestCompilersClick
          OnDrawItem = clbTestCompilersDrawItem
        end
        object btnStartTest: TButton
          Left = 240
          Top = 33
          Width = 97
          Height = 25
          Hint = 'Install each packed package into the cache to test it'
          Caption = 'Start Test'
          Enabled = False
          TabOrder = 1
          OnClick = btnStartTestClick
        end
        object btnCancelTest: TButton
          Left = 240
          Top = 64
          Width = 97
          Height = 25
          Hint = 'Cancel the running test (Esc)'
          Caption = 'Cancel'
          Enabled = False
          TabOrder = 2
          OnClick = btnCancelTestClick
        end
      end
      object TestLogMemo: TMemo
        Left = 0
        Top = 200
        Width = 878
        Height = 338
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        ExplicitWidth = 863
        ExplicitHeight = 328
      end
    end
    object tsUpload: TTabSheet
      Caption = 'Upload'
      ImageIndex = 6
      object pnlUploadTop: TPanel
        Left = 0
        Top = 0
        Width = 878
        Height = 233
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 863
        object lblUploadSource: TLabel
          Left = 16
          Top = 10
          Width = 36
          Height = 15
          Caption = 'Source'
        end
        object lblUploadApiKey: TLabel
          Left = 336
          Top = 10
          Width = 40
          Height = 15
          Caption = 'API Key'
        end
        object lblUploadPackages: TLabel
          Left = 16
          Top = 90
          Width = 216
          Height = 15
          Caption = 'Packages to upload (from output folder):'
        end
        object cboUploadSource: TComboBox
          Left = 16
          Top = 28
          Width = 300
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          OnChange = cboUploadSourceChange
        end
        object edtUploadApiKey: TEdit
          Left = 336
          Top = 28
          Width = 300
          Height = 23
          PasswordChar = '*'
          TabOrder = 1
        end
        object chkUploadSkipDuplicate: TCheckBox
          Left = 16
          Top = 62
          Width = 300
          Height = 17
          Caption = 'Skip packages that already exist on the source'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkUploadUnlisted: TCheckBox
          Left = 336
          Top = 62
          Width = 300
          Height = 17
          Caption = 'Upload as unlisted (not shown in package list)'
          TabOrder = 8
        end
        object rgUploadScope: TRadioGroup
          Left = 648
          Top = 6
          Width = 201
          Height = 73
          Caption = 'Upload'
          ItemIndex = 0
          Items.Strings = (
            'Current package (this dspec)'
            'All packages in output folder')
          TabOrder = 3
          OnClick = rgUploadScopeClick
        end
        object lstUploadPackages: TListBox
          Left = 16
          Top = 108
          Width = 620
          Height = 80
          ItemHeight = 15
          TabOrder = 4
        end
        object btnRefreshPackages: TButton
          Left = 648
          Top = 108
          Width = 97
          Height = 25
          Caption = 'Refresh'
          TabOrder = 5
          OnClick = btnRefreshPackagesClick
        end
        object btnUpload: TButton
          Left = 16
          Top = 198
          Width = 97
          Height = 25
          Caption = 'Upload'
          TabOrder = 6
          OnClick = btnUploadClick
        end
        object btnCancelUpload: TButton
          Left = 120
          Top = 198
          Width = 97
          Height = 25
          Hint = 'Cancel the running upload (Esc)'
          Caption = 'Cancel'
          Enabled = False
          TabOrder = 7
          OnClick = btnCancelUploadClick
        end
      end
      object UploadLogMemo: TMemo
        Left = 0
        Top = 233
        Width = 878
        Height = 305
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        ExplicitWidth = 863
        ExplicitHeight = 295
      end
    end
    object tsLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 4
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 878
        Height = 538
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 863
        ExplicitHeight = 528
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 568
    Width = 886
    Height = 19
    Panels = <>
    ExplicitLeft = 640
    ExplicitTop = -24
    ExplicitWidth = 0
  end
  object MainMenu: TMainMenu
    Left = 444
    Top = 466
    object mnuFile: TMenuItem
      Caption = '&File'
      object miNew: TMenuItem
        Action = actFileNew
      end
      object miPackageWizard: TMenuItem
        Action = actFilePackageWizard
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
    DefaultExt = 'dspec'
    Filter = 'Delphi Package Manager Spec Files|*.dspec;*.dspec.yaml'
    Left = 812
    Top = 386
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'dspec'
    Filter = 'Delphi Package Manager Spec Files|*.dspec;*.dspec.yaml'
    Left = 740
    Top = 386
  end
  object PopupMenu: TPopupMenu
    Left = 644
    Top = 442
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
    object actMoveEntryUp: TAction
      Category = 'Template'
      Caption = 'Move &Up'
      ShortCut = 16422
      OnExecute = actMoveEntryUpExecute
    end
    object actMoveEntryDown: TAction
      Category = 'Template'
      Caption = 'Move &Down'
      ShortCut = 16424
      OnExecute = actMoveEntryDownExecute
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
    object actFilePackageWizard: TAction
      Category = 'File'
      Caption = 'Package &Wizard...'
      OnExecute = actFilePackageWizardExecute
    end
    object actDeleteBuildItem: TAction
      Category = 'Build'
      Caption = 'Delect Build Item'
      OnExecute = actDeleteBuildItemExecute
    end
    object actAddCopyLocalItem: TAction
      Category = 'CopyLocal'
      Caption = 'Add CopyLocal Item'
      OnExecute = actAddCopyLocalItemExecute
    end
    object actDeleteCopyLocalItem: TAction
      Category = 'CopyLocal'
      Caption = 'Delete CopyLocal Item'
      OnExecute = actDeleteCopyLocalItemExecute
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
    object actAddPackageDefItem: TAction
      Category = 'PackageDef'
      Caption = 'Add Package Definition'
      OnExecute = actAddPackageDefItemExecute
    end
    object actDeletePackageDefItem: TAction
      Category = 'PackageDef'
      Caption = 'Delete Package Definition'
      OnExecute = actDeletePackageDefItemExecute
    end
    object actAddSourceItem: TAction
      Category = 'Source'
      Caption = 'Add Source Item'
      OnExecute = actAddSourceItemExecute
    end
    object actDeleteSourceItem: TAction
      Category = 'Source'
      Caption = 'Delete Source Item'
      OnExecute = actDeleteSourceItemExecute
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
    Left = 524
    Top = 506
    object SelectAll1: TMenuItem
      Action = actCompilersSelectAll
    end
    object DeselectAll1: TMenuItem
      Action = actCompilersDeselectAll
    end
  end
  object pmPlatforms: TPopupMenu
    Left = 444
    Top = 506
    object SelectAll2: TMenuItem
      Action = actPlatformsSelectAll
    end
    object DeselectAll2: TMenuItem
      Action = actPlatformsDeselectAll
    end
  end
end
