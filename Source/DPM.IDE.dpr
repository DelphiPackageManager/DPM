{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

library DPM.IDE;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }









{$R *.dres}

uses
  System.SysUtils,
  System.Classes,
  DPM.IDE.Main in 'IDE\DPM.IDE.Main.pas',
  DPM.IDE.Wizard in 'IDE\DPM.IDE.Wizard.pas',
  DPM.IDE.ProjectStorageNotifier in 'IDE\DPM.IDE.ProjectStorageNotifier.pas',
  DPM.IDE.Logger in 'IDE\DPM.IDE.Logger.pas',
  DPM.IDE.IDENotifier in 'IDE\DPM.IDE.IDENotifier.pas',
  DPM.IDE.ProjectMenu in 'IDE\DPM.IDE.ProjectMenu.pas',
  DPM.IDE.Constants in 'IDE\DPM.IDE.Constants.pas',
  DPM.IDE.EditorView in 'IDE\DPM.IDE.EditorView.pas',
  DPM.IDE.EditorViewFrame in 'IDE\DPM.IDE.EditorViewFrame.pas' {DPMEditViewFrame: TFrame},
  DPM.IDE.EditorViewManager in 'IDE\DPM.IDE.EditorViewManager.pas',
  DPM.IDE.AddInOptions in 'IDE\DPM.IDE.AddInOptions.pas',
  DPM.IDE.AddInOptionsFrame in 'IDE\DPM.IDE.AddInOptionsFrame.pas' {DPMOptionsFrame: TFrame},
  DPM.Controls.ButtonedEdit in 'Controls\DPM.Controls.ButtonedEdit.pas',
  DPM.Controls.AutoComplete in 'Controls\DPM.Controls.AutoComplete.pas',
  DPM.IDE.AboutForm in 'IDE\DPM.IDE.AboutForm.pas' {DPMAboutForm},
  DPM.IDE.Types in 'IDE\DPM.IDE.Types.pas',
  DPM.Controls.GroupButton in 'Controls\DPM.Controls.GroupButton.pas',
  DPM.IDE.AddInOptionsHostForm in 'IDE\DPM.IDE.AddInOptionsHostForm.pas' {DPMOptionsHostForm},
  DPM.IDE.IconCache in 'IDE\DPM.IDE.IconCache.pas',
  DPM.IDE.PackageDetailsFrame in 'IDE\DPM.IDE.PackageDetailsFrame.pas' {PackageDetailsFrame: TFrame},
  DPM.IDE.PackageDetailsPanel in 'IDE\DPM.IDE.PackageDetailsPanel.pas';

{$R *.res}

begin
end.
