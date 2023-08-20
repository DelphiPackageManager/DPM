object DPMOptionsFrame: TDPMOptionsFrame
  Left = 0
  Top = 0
  Width = 668
  Height = 504
  TabOrder = 0
  object pgOptions: TPageControl
    Left = 0
    Top = 0
    Width = 668
    Height = 504
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tsSources
    Align = alClient
    TabHeight = 24
    TabOrder = 0
    object tsSources: TTabSheet
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Caption = ' Package Sources '
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 660
        Height = 89
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          660
          89)
        object lblPackageSources: TLabel
          Left = 16
          Top = 61
          Width = 108
          Height = 13
          Caption = ' DPM Package Sources'
        end
        object Label3: TLabel
          Left = 16
          Top = 12
          Width = 109
          Height = 13
          Alignment = taRightJustify
          Caption = 'Package cache folder :'
        end
        object btnAdd: TSpeedButton
          Left = 477
          Top = 61
          Width = 41
          Height = 24
          Action = actAddSource
          Anchors = [akTop, akRight]
          Caption = '+'
          ParentShowHint = False
          ShowHint = True
        end
        object btnRemove: TSpeedButton
          Left = 524
          Top = 61
          Width = 41
          Height = 24
          Action = actRemoveSource
          Anchors = [akTop, akRight]
          Caption = '-'
          ParentShowHint = False
          ShowHint = True
          ExplicitLeft = 565
        end
        object btnUp: TSpeedButton
          Left = 569
          Top = 61
          Width = 41
          Height = 24
          Action = actMoveSourceUp
          Anchors = [akTop, akRight]
          Caption = 'up'
          ParentShowHint = False
          ShowHint = True
          ExplicitLeft = 610
        end
        object btnDown: TSpeedButton
          Left = 614
          Top = 61
          Width = 41
          Height = 24
          Action = actMoveSourceDown
          Anchors = [akTop, akRight]
          Caption = 'dn'
          ParentShowHint = False
          ShowHint = True
          ExplicitLeft = 655
        end
        object txtPackageCacheLocation: TButtonedEdit
          Left = 16
          Top = 28
          Width = 409
          Height = 21
          DoubleBuffered = True
          ParentDoubleBuffered = False
          RightButton.Hint = 'Sected Folder'
          RightButton.ImageIndex = 4
          RightButton.Visible = True
          TabOrder = 0
          OnRightButtonClick = txtPackageCacheLocationRightButtonClick
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 259
        Width = 660
        Height = 211
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 24
          Top = 6
          Width = 70
          Height = 13
          Caption = 'Source Name :'
        end
        object Label2: TLabel
          Left = 24
          Top = 49
          Width = 94
          Height = 13
          Caption = 'Source Path or Uri :'
        end
        object Label5: TLabel
          Left = 24
          Top = 97
          Width = 92
          Height = 13
          Caption = 'Source UserName :'
        end
        object Label6: TLabel
          Left = 24
          Top = 145
          Width = 158
          Height = 13
          Caption = 'Source Password/Access Token :'
        end
        object Label8: TLabel
          Left = 481
          Top = 6
          Width = 67
          Height = 13
          Caption = 'Source Type :'
        end
        object txtName: TEdit
          Left = 24
          Top = 22
          Width = 409
          Height = 21
          TabOrder = 0
          OnChange = txtNameChange
        end
        object txtUri: TButtonedEdit
          Left = 24
          Top = 65
          Width = 409
          Height = 21
          RightButton.ImageIndex = 4
          RightButton.Visible = True
          TabOrder = 1
          OnChange = txtUriChange
          OnRightButtonClick = txtUriRightButtonClick
        end
        object txtUserName: TEdit
          Left = 24
          Top = 113
          Width = 409
          Height = 21
          TabOrder = 2
          OnChange = txtUserNameChange
        end
        object txtPassword: TEdit
          Left = 24
          Top = 161
          Width = 409
          Height = 21
          PasswordChar = '*'
          TabOrder = 3
          OnChange = txtPasswordChange
        end
        object cboSourceType: TComboBox
          Left = 480
          Top = 22
          Width = 171
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 4
          Text = 'Folder'
          OnChange = cboSourceTypeChange
          Items.Strings = (
            'Folder'
            'DPMServer'
            'DPMGithub'
            'DNGithub')
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 89
        Width = 660
        Height = 170
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel3'
        TabOrder = 2
        object lvSources: TListView
          Left = 0
          Top = 0
          Width = 660
          Height = 170
          Align = alClient
          BevelInner = bvNone
          Checkboxes = True
          Columns = <
            item
              Caption = 'Name'
              MinWidth = 100
              Width = 100
            end
            item
              AutoSize = True
              Caption = 'Uri'
            end
            item
              Caption = 'Type'
              MinWidth = 100
              Width = 100
            end
            item
              AutoSize = True
              Caption = 'UserName'
              MinWidth = 100
            end>
          ColumnClick = False
          RowSelect = True
          ShowWorkAreas = True
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lvSourcesSelectItem
        end
      end
    end
    object tsIDEOptions: TTabSheet
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Caption = ' IDE Options '
      ImageIndex = 1
      object pnlIDEOptions: TPanel
        Left = 0
        Top = 0
        Width = 660
        Height = 470
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlIDEOptions'
        ShowCaption = False
        TabOrder = 0
        object Label4: TLabel
          Left = 24
          Top = 24
          Width = 65
          Height = 13
          Caption = 'Logging Level'
        end
        object Label7: TLabel
          Left = 24
          Top = 88
          Width = 95
          Height = 13
          Caption = 'Show Log View for :'
        end
        object Label9: TLabel
          Left = 264
          Top = 131
          Width = 132
          Height = 13
          Caption = 'Auto Close Delay (seconds)'
        end
        object Label10: TLabel
          Left = 24
          Top = 196
          Width = 79
          Height = 13
          Caption = 'Project Manager'
        end
        object Label11: TLabel
          Left = 56
          Top = 238
          Width = 239
          Height = 13
          Caption = 'Experimental - very slow for large project groups.'
          Enabled = False
        end
        object chkShowForInstall: TCheckBox
          Left = 40
          Top = 132
          Width = 97
          Height = 17
          Caption = 'Install'
          TabOrder = 0
        end
        object chkShowForRestore: TCheckBox
          Left = 40
          Top = 107
          Width = 97
          Height = 17
          Caption = 'Restore'
          TabOrder = 1
        end
        object chkShowForUninstall: TCheckBox
          Left = 40
          Top = 158
          Width = 97
          Height = 17
          Caption = 'Uninstall'
          TabOrder = 2
        end
        object spAutoCloseDelay: TSpinEdit
          Left = 264
          Top = 150
          Width = 121
          Height = 22
          MaxValue = 30
          MinValue = 0
          TabOrder = 3
          Value = 0
        end
        object chkAutoClose: TCheckBox
          Left = 240
          Top = 107
          Width = 177
          Height = 17
          Caption = 'Auto Close Log View on success'
          TabOrder = 4
          OnClick = chkAutoCloseClick
        end
        object cboLogLevel: TComboBox
          Left = 24
          Top = 43
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 5
          Text = 'Normal'
          Items.Strings = (
            'Quiet'
            'Normal'
            'Detailed'
            'Debug')
        end
        object chkShowOnProjectTree: TCheckBox
          Left = 40
          Top = 215
          Width = 241
          Height = 17
          Caption = 'Show DPM Node in Project Manager'
          TabOrder = 6
        end
      end
    end
  end
  object dpmOptionsActionList: TActionList
    OnUpdate = dpmOptionsActionListUpdate
    Left = 576
    Top = 128
    object actAddSource: TAction
      Hint = 'Add Source'
      ImageIndex = 0
      OnExecute = actAddSourceExecute
    end
    object actRemoveSource: TAction
      Hint = 'Remove Source'
      ImageIndex = 1
      OnExecute = actRemoveSourceExecute
    end
    object actMoveSourceUp: TAction
      Hint = 'Move Up'
      ImageIndex = 2
      OnExecute = actMoveSourceUpExecute
    end
    object actMoveSourceDown: TAction
      Hint = 'Move Down'
      ImageIndex = 3
      OnExecute = actMoveSourceDownExecute
    end
  end
  object FolderSelectDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist, fdoCreatePrompt, fdoShareAware]
    Title = 'Select Folder'
    Left = 560
    Top = 376
  end
end
