object PackageWizardForm: TPackageWizardForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Package Wizard'
  ClientHeight = 420
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 524
    Height = 371
    ActivePage = tsRoot
    Align = alClient
    TabOrder = 0
    object tsRoot: TTabSheet
      Caption = 'Root'
      object lblRootIntro: TLabel
        Left = 16
        Top = 16
        Width = 480
        Height = 45
        AutoSize = False
        Caption =
          'This wizard scaffolds a .dspec.yaml file from an existing source ' +
          'tree, the same way the dpm "spec" command does. Choose the proje' +
          'ct root folder to begin.'
        WordWrap = True
      end
      object lblRoot: TLabel
        Left = 16
        Top = 72
        Width = 96
        Height = 15
        Caption = 'Project root folder:'
      end
      object edtRootFolder: TEdit
        Left = 16
        Top = 92
        Width = 400
        Height = 23
        TabOrder = 0
      end
      object btnBrowseRoot: TButton
        Left = 424
        Top = 91
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseRootClick
      end
    end
    object tsFolders: TTabSheet
      Caption = 'Folders'
      ImageIndex = 1
      object lblSourceFolder: TLabel
        Left = 16
        Top = 16
        Width = 71
        Height = 15
        Caption = 'Source folder:'
      end
      object lblPackagesFolder: TLabel
        Left = 16
        Top = 112
        Width = 86
        Height = 15
        Caption = 'Packages folder:'
      end
      object edtSourceFolder: TEdit
        Left = 16
        Top = 36
        Width = 400
        Height = 23
        TabOrder = 0
      end
      object btnBrowseSource: TButton
        Left = 424
        Top = 35
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseSourceClick
      end
      object chkHasPackages: TCheckBox
        Left = 16
        Top = 80
        Width = 480
        Height = 17
        Caption = 'This project has package (.dproj) projects'
        TabOrder = 2
        OnClick = chkHasPackagesClick
      end
      object edtPackagesFolder: TEdit
        Left = 16
        Top = 132
        Width = 400
        Height = 23
        TabOrder = 3
      end
      object btnBrowsePackages: TButton
        Left = 424
        Top = 131
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 4
        OnClick = btnBrowsePackagesClick
      end
    end
    object tsMetadata: TTabSheet
      Caption = 'Metadata'
      ImageIndex = 2
      object lblId: TLabel
        Left = 16
        Top = 16
        Width = 57
        Height = 15
        Caption = 'Package id:'
      end
      object lblDescription: TLabel
        Left = 16
        Top = 72
        Width = 63
        Height = 15
        Caption = 'Description:'
      end
      object lblAuthor: TLabel
        Left = 16
        Top = 128
        Width = 40
        Height = 15
        Caption = 'Author:'
      end
      object lblVersion: TLabel
        Left = 16
        Top = 184
        Width = 43
        Height = 15
        Caption = 'Version:'
      end
      object lblLicense: TLabel
        Left = 200
        Top = 184
        Width = 99
        Height = 15
        Caption = 'License (SPDX id):'
      end
      object edtId: TEdit
        Left = 16
        Top = 36
        Width = 300
        Height = 23
        TabOrder = 0
      end
      object edtDescription: TEdit
        Left = 16
        Top = 92
        Width = 483
        Height = 23
        TabOrder = 1
      end
      object edtAuthor: TEdit
        Left = 16
        Top = 148
        Width = 300
        Height = 23
        TabOrder = 2
      end
      object edtVersion: TEdit
        Left = 16
        Top = 204
        Width = 140
        Height = 23
        TabOrder = 3
      end
      object cboLicense: TComboBox
        Left = 200
        Top = 204
        Width = 300
        Height = 23
        TabOrder = 4
      end
    end
    object tsMultiPackage: TTabSheet
      Caption = 'Multi-Package'
      ImageIndex = 3
      object lblWhich: TLabel
        Left = 32
        Top = 124
        Width = 50
        Height = 15
        Caption = 'Package:'
      end
      object rgMultiMode: TRadioGroup
        Left = 16
        Top = 16
        Width = 483
        Height = 90
        Caption = 'Multiple package projects detected'
        TabOrder = 0
        OnClick = rgMultiModeClick
      end
      object cboWhichPackage: TComboBox
        Left = 32
        Top = 144
        Width = 300
        Height = 23
        Style = csDropDownList
        TabOrder = 1
      end
    end
    object tsReview: TTabSheet
      Caption = 'Review'
      ImageIndex = 4
      object lblReview: TLabel
        Left = 16
        Top = 12
        Width = 49
        Height = 15
        Caption = 'Summary:'
      end
      object mmoReview: TMemo
        Left = 16
        Top = 36
        Width = 483
        Height = 295
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 371
    Width = 524
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnBack: TButton
      Left = 184
      Top = 12
      Width = 75
      Height = 25
      Caption = '< Back'
      TabOrder = 0
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 265
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Next >'
      Default = True
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnFinish: TButton
      Left = 265
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Finish'
      TabOrder = 2
      Visible = False
      OnClick = btnFinishClick
    end
    object btnCancel: TButton
      Left = 424
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btnCancelClick
    end
  end
end
