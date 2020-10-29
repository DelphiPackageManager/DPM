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
  DPM.Core.Types,
  DPM.Core.Logging;

type
  IDPMIDELogger = interface(ILogger)
    ['{02CA41D0-F46A-4FB7-A743-DFCFA3E0EAD9}']
    procedure ShowMessageTab;
    procedure StartRestore;
    procedure EndRestore;
    procedure StartProject(const fileName : string; const msg : string = '');
    procedure EndProject(const fileName : string; const msg : string = '');
  end;

  TDPMIDELogger = class(TInterfacedObject, ILogger, IDPMIDELogger)
  private
    FMessageServices : IOTAMessageServices;
    FMessageGroup : IOTAMessageGroup;
    FVerbosity : TVerbosity;
  protected
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);

    procedure Clear;
    procedure ShowMessageTab;
    procedure StartProject(const fileName : string; const msg : string = '');
    procedure EndProject(const fileName : string; const msg : string = '');
    procedure StartRestore;
    procedure EndRestore;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils,
  System.Classes;

{ TDPMIDELogger }

procedure TDPMIDELogger.Clear;
begin
  FMessageServices.ClearMessageGroup(FMessageGroup);
end;

constructor TDPMIDELogger.Create;
begin
  FMessageServices := BorlandIDEServices as IOTAMessageServices;
  FMessageGroup := FMessageServices.AddMessageGroup('DPM');
  FMessageGroup.CanClose := false;
  FMessageGroup.AutoScroll := true;
  FVerbosity := TVerbosity.Debug; //TODO : Need to make this configurable
end;

procedure TDPMIDELogger.Debug(const data : string);
var
  lineRef : Pointer;
  debugProc : TThreadProcedure;
begin
  if (FVerbosity < TVerbosity.Debug) then
    exit;


  debugProc := procedure
  begin
    if FMessageServices <> nil then
      FMessageServices.AddToolMessage('', 'DEBUG: ' + data, '', 0, 0, nil, lineRef, FMessageGroup)
  end;

  //FMessageServices is implemented by a vcl control, so we need to ensure it's only updated by the main thread.
  if TThread.CurrentThread.ThreadID = MainThreadID then
    debugProc
  else
    TThread.Synchronize(nil, debugProc);
end;

destructor TDPMIDELogger.Destroy;
begin
  FMessageGroup := nil;
  FMessageServices := nil;
  inherited;
end;

procedure TDPMIDELogger.EndProject(const fileName : string; const msg : string);
var
  lineRef : Pointer;
begin
  FMessageServices.AddToolMessage(fileName, 'Done.' + msg, '', 0, 0, nil, lineRef, FMessageGroup);

end;

procedure TDPMIDELogger.EndRestore;
begin
  //  FMessageServices.AddTitleMessage('DPM Restore done.', FMessageGroup);
end;

procedure TDPMIDELogger.Error(const data : string);
var
  lineRef : Pointer;
  errorProc : TThreadProcedure;
begin
  //TODO : Send custom message so we can color it etc
  errorProc := procedure
  begin
    FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
  end;

  if TThread.CurrentThread.ThreadID = MainThreadID then
    errorProc
  else
    TThread.Queue(nil, errorProc);

end;

function TDPMIDELogger.GetVerbosity : TVerbosity;
begin
  result := FVerbosity;
end;

procedure TDPMIDELogger.Information(const data : string; const important : Boolean);
var
  lineRef : Pointer;
  infoProc : TThreadProcedure;
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;

  infoProc := procedure
  begin
    FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
  end;

  if TThread.CurrentThread.ThreadID = MainThreadID then
    infoProc
  else
    TThread.Queue(nil, infoProc);



end;

procedure TDPMIDELogger.SetVerbosity(const value : TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDPMIDELogger.ShowMessageTab;
begin
  FMessageServices.ShowMessageView(FMessageGroup);
end;

procedure TDPMIDELogger.StartProject(const fileName : string; const msg : string);
var
  lineRef : Pointer;
begin
  FMessageServices.AddToolMessage(fileName, 'Restoring packages...' + msg, '', 0, 0, nil, lineRef, FMessageGroup);
end;

procedure TDPMIDELogger.StartRestore;
begin
  //  FMessageServices.AddTitleMessage('Restoring DPM packages', FMessageGroup);
end;

procedure TDPMIDELogger.Success(const data: string; const important: Boolean);
var
  lineRef : Pointer;
  infoProc : TThreadProcedure;
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;

  infoProc := procedure
  begin
    FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
  end;

  if TThread.CurrentThread.ThreadID = MainThreadID then
    infoProc
  else
    TThread.Queue(nil, infoProc);
end;

procedure TDPMIDELogger.Verbose(const data : string; const important : Boolean);
var
  lineRef : Pointer;
  verboseProc : TThreadProcedure;
begin
  if (FVerbosity < TVerbosity.Detailed) then
    exit;

  verboseProc := procedure
  begin
    FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
  end;

  if TThread.CurrentThread.ThreadID = MainThreadID then
    verboseProc
  else
    TThread.Queue(nil, verboseProc);

end;

procedure TDPMIDELogger.Warning(const data : string; const important : Boolean);
var
  lineRef : Pointer;
  warningProc : TThreadProcedure;
begin

  warningProc := procedure
  begin
    FMessageServices.AddToolMessage('', data, '', 0, 0, nil, lineRef, FMessageGroup);
  end;

  if TThread.CurrentThread.ThreadID = MainThreadID then
    warningProc
  else
    TThread.Queue(nil, warningProc);
end;

end.

