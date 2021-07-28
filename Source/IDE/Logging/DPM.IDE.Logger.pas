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


unit DPM.IDE.Logger;

interface

uses
  ToolsApi,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.IDE.MessageService;

type
  IDPMIDELogger = interface(ILogger)
    ['{02CA41D0-F46A-4FB7-A743-DFCFA3E0EAD9}']
    procedure ShowMessageTab;

    procedure StartRestore(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndRestore(const success  : boolean);
    procedure StartInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndInstall(const success  : boolean);
    procedure StartUnInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndUnInstall(const success  : boolean);

    procedure StartProject(const fileName : string; const msg : string = '');
    procedure EndProject(const fileName : string; const msg : string = '');
  end;

  TDPMIDELogger = class(TInterfacedObject, IDPMIDELogger, ILogger )
  private
    {$IFDEF DEBUG}
    FMessageServices : IOTAMessageServices;
    FMessageGroup : IOTAMessageGroup;
    {$ENDIF}
    FVerbosity : TVerbosity;
    FDPMMessageService : IDPMIDEMessageService;
  protected
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);

    procedure Clear;
    procedure ShowMessageTab;

    procedure StartProject(const fileName : string; const msg : string = '');
    procedure EndProject(const fileName : string; const msg : string = '');

    procedure StartRestore(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndRestore(const success  : boolean);

    procedure StartInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndInstall(const success  : boolean);
    procedure StartUnInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndUnInstall(const success  : boolean);


  public
    constructor Create(const dpmMessageService : IDPMIDEMessageService);
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils,
  System.Classes;

{ TDPMIDELogger }

procedure TDPMIDELogger.Clear;
begin
{$IFDEF DEBUG}
  FMessageServices.ClearMessageGroup(FMessageGroup);
{$ENDIF}
  FDPMMessageService.Clear;
end;

constructor TDPMIDELogger.Create(const dpmMessageService : IDPMIDEMessageService);
begin
  FDPMMessageService := dpmMessageService;
  {$IFDEF DEBUG}
  FMessageServices := BorlandIDEServices as IOTAMessageServices;
  FMessageGroup := FMessageServices.AddMessageGroup('DPM');
  FMessageGroup.CanClose := false;
  FMessageGroup.AutoScroll := true;
  {$ENDIF}
  FVerbosity := TVerbosity.Debug; //TODO : Need to make this configurable
end;

procedure TDPMIDELogger.Debug(const data : string);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
  if (FVerbosity < TVerbosity.Debug) then
    exit;
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Debug(data);
end;

destructor TDPMIDELogger.Destroy;
begin
{$IFDEF DEBUG}
  FMessageGroup := nil;
  FMessageServices := nil;
{$ENDIF}
  inherited;
end;

procedure TDPMIDELogger.EndInstall(const success  : boolean);
begin
  FDPMMessageService.TaskDone(success);
end;

procedure TDPMIDELogger.EndProject(const fileName : string; const msg : string);
{$IFDEF DEBUG}
//var
//  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
//  FMessageServices.AddToolMessage(fileName, 'Done.' + msg, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
end;

procedure TDPMIDELogger.EndRestore(const success  : boolean);
begin
{$IFDEF DEBUG}
  FMessageServices.AddTitleMessage('DPM Restore done.', FMessageGroup);
{$ENDIF}
  FDPMMessageService.TaskDone(success);
end;

procedure TDPMIDELogger.EndUnInstall(const success  : boolean);
begin
{$IFDEF DEBUG}
  FMessageServices.AddTitleMessage('DPM Install done.', FMessageGroup);
{$ENDIF}
  FDPMMessageService.TaskDone(success);
end;

procedure TDPMIDELogger.Error(const data : string);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', 'ERR: ' + data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Error(data);
end;

function TDPMIDELogger.GetVerbosity : TVerbosity;
begin
  result := FVerbosity;
end;

procedure TDPMIDELogger.Information(const data : string; const important : Boolean);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Information(data, important);
end;

procedure TDPMIDELogger.NewLine;
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', ' ', '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.NewLine;
end;

procedure TDPMIDELogger.SetVerbosity(const value : TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDPMIDELogger.ShowMessageTab;
begin
{$IFDEF DEBUG}
 FMessageServices.ShowMessageView(FMessageGroup);
{$ENDIF}
end;

procedure TDPMIDELogger.StartInstall(const cancellationTokenSource : ICancellationTokenSource);
begin
  FDPMMessageService.TaskStarted(cancellationTokenSource, mtInstall);
end;

procedure TDPMIDELogger.StartProject(const fileName : string; const msg : string);
//var
//  lineRef : Pointer;
begin
{$IFDEF DEBUG}
  ////FMessageServices.AddToolMessage(fileName, 'Restoring packages...' + msg, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
end;

procedure TDPMIDELogger.StartRestore(const cancellationTokenSource : ICancellationTokenSource);
begin
{$IFDEF DEBUG}
  FMessageServices.AddTitleMessage('Restoring DPM packages', FMessageGroup);
{$ENDIF}
  FDPMMessageService.TaskStarted(cancellationTokenSource, mtRestore);
end;

procedure TDPMIDELogger.StartUnInstall(const cancellationTokenSource : ICancellationTokenSource);
begin
  FDPMMessageService.TaskStarted(cancellationTokenSource, mtUninstall);
end;

procedure TDPMIDELogger.Success(const data: string; const important: Boolean);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Success(data, important);
end;

procedure TDPMIDELogger.Verbose(const data : string; const important : Boolean);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
  if (FVerbosity < TVerbosity.Detailed) then
    exit;
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Verbose(data, important);
end;

procedure TDPMIDELogger.Warning(const data : string; const important : Boolean);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  FDPMMessageService.Warning(data, important);
end;

end.

