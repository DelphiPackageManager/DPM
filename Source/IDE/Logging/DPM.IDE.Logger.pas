{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

    procedure StartVerifyCache(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndVerifyCache(const success  : boolean);

  end;

  TDPMIDELogger = class(TBaseLogger, IDPMIDELogger, ILogger)
  private
    {$IFDEF DEBUG}
    FMessageServices : IOTAMessageServices;
    FMessageGroup : IOTAMessageGroup;
    {$ENDIF}
    FDPMMessageService : IDPMIDEMessageService;
    procedure TaskStarted(const cancellationTokenSource : ICancellationTokenSource; const task : TMessageTask);
  protected
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); override;
    procedure DoClear; override;
    procedure DoNewLine; override;

    procedure ShowMessageTab;

    procedure StartProject(const fileName : string; const msg : string = '');
    procedure EndProject(const fileName : string; const msg : string = '');

    procedure StartRestore(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndRestore(const success  : boolean);

    procedure StartInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndInstall(const success  : boolean);
    procedure StartUnInstall(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndUnInstall(const success  : boolean);

    procedure StartVerifyCache(const cancellationTokenSource : ICancellationTokenSource);
    procedure EndVerifyCache(const success  : boolean);


  public
    constructor Create(const dpmMessageService : IDPMIDEMessageService);
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils,
  System.Classes;

{ TDPMIDELogger }

constructor TDPMIDELogger.Create(const dpmMessageService : IDPMIDEMessageService);
begin
  inherited Create;
  FDPMMessageService := dpmMessageService;
  {$IFDEF DEBUG}
  FMessageServices := BorlandIDEServices as IOTAMessageServices;
  FMessageGroup := FMessageServices.AddMessageGroup('DPM');
  FMessageGroup.CanClose := false;
  FMessageGroup.AutoScroll := true;
  {$ENDIF}
  Verbosity := TVerbosity.Normal; // we can set this from the options.
end;

destructor TDPMIDELogger.Destroy;
begin
{$IFDEF DEBUG}
  FMessageGroup := nil;
  FMessageServices := nil;
{$ENDIF}
  inherited;
end;

procedure TDPMIDELogger.WriteMessage(const level : TLogLevel; const text : string; const important : boolean);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if level = TLogLevel.Debug then
    FMessageServices.AddToolMessage('', 'DEBUG: ' + text, '', 0, 0, nil, lineRef, FMessageGroup)
  else
    FMessageServices.AddToolMessage('', text, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
  case level of
    TLogLevel.Debug : FDPMMessageService.Debug('DEBUG: ' + text);
    TLogLevel.Verbose : FDPMMessageService.Verbose(text, important);
    TLogLevel.Information : FDPMMessageService.Information(text, important);
    TLogLevel.Success : FDPMMessageService.Success(text, important);
    TLogLevel.Warning : FDPMMessageService.Warning(text, important);
    TLogLevel.Error : FDPMMessageService.Error(text);
  end;
end;

procedure TDPMIDELogger.DoClear;
begin
{$IFDEF DEBUG}
  FMessageServices.ClearMessageGroup(FMessageGroup);
{$ENDIF}
  FDPMMessageService.Clear;
end;

procedure TDPMIDELogger.DoNewLine;
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

procedure TDPMIDELogger.TaskStarted(const cancellationTokenSource : ICancellationTokenSource; const task : TMessageTask);
begin
  //a new task - an error the previous task reported must show again if this one hits it too.
  StartDiagnosticScope;
  FDPMMessageService.TaskStarted(cancellationTokenSource, task);
end;

procedure TDPMIDELogger.EndInstall(const success  : boolean);
begin
  FDPMMessageService.TaskDone(success);
end;

procedure TDPMIDELogger.EndProject(const fileName : string; const msg : string);
{$IFDEF DEBUG}
var
  lineRef : Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage(fileName, 'Done.' + msg, '', 0, 0, nil, lineRef, FMessageGroup);
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

procedure TDPMIDELogger.ShowMessageTab;
begin
{$IFDEF DEBUG}
 FMessageServices.ShowMessageView(FMessageGroup);
{$ENDIF}
end;

procedure TDPMIDELogger.StartInstall(const cancellationTokenSource : ICancellationTokenSource);
begin
  TaskStarted(cancellationTokenSource, mtInstall);
end;

procedure TDPMIDELogger.StartProject(const fileName : string; const msg : string);
var
  lineRef : Pointer;
begin
{$IFDEF DEBUG}
  FMessageServices.AddToolMessage(fileName, 'Restoring packages...' + msg, '', 0, 0, nil, lineRef, FMessageGroup);
{$ENDIF}
end;

procedure TDPMIDELogger.StartRestore(const cancellationTokenSource : ICancellationTokenSource);
begin
{$IFDEF DEBUG}
  FMessageServices.AddTitleMessage('Restoring DPM packages', FMessageGroup);
{$ENDIF}
  TaskStarted(cancellationTokenSource, mtRestore);
end;

procedure TDPMIDELogger.StartUnInstall(const cancellationTokenSource : ICancellationTokenSource);
begin
  TaskStarted(cancellationTokenSource, mtUninstall);
end;

procedure TDPMIDELogger.StartVerifyCache(const cancellationTokenSource : ICancellationTokenSource);
begin
  TaskStarted(cancellationTokenSource, mtVerifyCache);
end;

procedure TDPMIDELogger.EndVerifyCache(const success : boolean);
begin
  FDPMMessageService.TaskDone(success);
end;

end.
