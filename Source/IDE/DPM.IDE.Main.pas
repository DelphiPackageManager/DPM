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

unit DPM.IDE.Main;

interface

uses
  ToolsAPI,
  System.SysUtils,
  Vcl.Dialogs,
  DPM.IDE.Wizard;

function InitWizard(const BorlandIDEServices : IBorlandIDEServices; RegisterProc : TWizardRegisterProc; var Terminate : TWizardTerminateProc) : Boolean; stdcall;

exports
  InitWizard name ToolsAPI.WizardEntryPoint;

implementation

uses
  WinApi.ActiveX,
  WinApi.Windows,
  System.Classes,
  System.TypInfo,
  System.Rtti,
  System.Win.Registry,
  Vcl.Graphics,
  Vcl.Forms,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Process,
  DPM.IDE.Constants;

var
  SplashImage : TBitmap;
  wizardIdx : integer = -1;


procedure DumpMethod(rttiMethod : TRttiMethod;const sl : TStringList  );

  function dumpParam(rttiParam : TRttiParameter) : string;
  begin
    result := rttiParam.Name + ' : ';
    if rttiParam.ParamType <> nil then
      result := result + rttiParam.ParamType.Name;
  end;

var
  params : TArray<TRttiParameter>;
  i : integer;
  sParams : string;
begin

  params := rttiMethod.GetParameters;
  if Length(params) > 0 then
  begin
    for i := Low(params) to High(params) do
    begin
      if sParams <> '' then
        sParams := sParams + ', ';
      sParams := sParams + dumpParam(params[i]);
    end;
  end;
  sl.Add(rttiMethod.Name + '(' + sParams + ')');
end;


procedure DumpField(rttiField : TRttiField; const sl : TStringList);
begin
  sl.Add(rttiField.Name + ' : ' + GetTypeName(rttiField.FieldType.Handle));
end;

procedure DumpType(rttiType : TRttiType; const sl : TStringList  );
var
  fields : TArray<TRttiField>;
  methods : TArray<TRttiMethod>;
  i : integer;
begin
  sl.Add(rttiType.Name);
  fields :=rttiType.GetFields;

  for I := Low(fields) to High(fields) do
    DumpField(fields[i], sl);

  methods := rttiType.GetDeclaredMethods;
  for I := Low(methods) to High(methods) do
      DumpMethod(methods[i], sl);
end;


//procedure DumpEnvironmentOptionNames;
//var
//  Opts: IOTAEnvironmentOptions;
//  rttiCtx : TRttiContext;
//  optType : TRttiInstanceType;
//  inst : TValue;
//  envOptsField : TRttiField;
//  envOptsType : TRttiType;
//  method : TRttiMethod;
//  paramTypes : TArray<TRttiParameter>;
//  params : TArray<TValue>;
//  optObj : TObject;
//  FEnvOptions : TValue;
//  sl : TStringList;
//  reg : TRegistry;
//  sBrowsePath : string;
//begin
//  Opts := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
//  if not Assigned(Opts) then Exit;
//  optObj := TObject(Opts);
//
//  rttiCtx := TRttiContext.Create;
//  optType := rttiCtx.GetType(optObj.ClassType) as TRttiInstanceType;
//
//  sl := TStringList.Create;
//  try
//    DumpType(optType, sl);
//
//
//    inst := TValue.From(optType.Handle, optObj);
//    envOptsField := optType.GetField('FEnvOptions');
//    envOptsType := envOptsField.FieldType;
//
//  //  DumpType(envOptsType, sl);
//
//
//    FEnvOptions :=  envOptsField.GetValue(optObj);
//
//
//    sBrowsePath:= Opts.Values['BrowsePath'];
//
//    Opts.Values['Browsing Path'] := sBrowsePath + ';$(DPMCACHE)\delphi13.0\GR32.graphics32\3.1.0\Source';
//
//
//    reg := TRegistry.Create;
//    try
//      reg.RootKey := HKEY_CURRENT_USER;
//      reg.OpenKey('Software\Embarcadero\DPMTesting\37.0\Environment Variables',true);
//      reg.WriteString('DPMCache', 'C:\Users\vincent.OFFICE\AppData\Roaming\.dpm\package_cache');
//      reg.WriteString('DPMBWin32', '$(DPMCACHE)\delphi13.0\GR32.graphics32\3.1.0\Source');
//      reg.CloseKey;
//
//      reg.OpenKey('Software\Embarcadero\DPMTesting\37.0\Library\Win32', true);
//      sBrowsePath := reg.ReadString('Browsing Path');
//      if Pos('$(DPMBWin32)',sBrowsePath) = -1 then
//      begin
//        sBrowsePath := '$(DPMBWin32);' + sBrowsePath;
//        reg.WriteString('Browsing Path',  sBrowsePath);
//      end;
//      reg.CloseKey;
//    finally
//      reg.Free;
//    end;
//
//    SetEnvironmentVariable('DPMCache', 'C:\Users\vincent.OFFICE\AppData\Roaming\.dpm\package_cache');
//    SetEnvironmentVariable('DPMBWin32', '$(DPMCACHE)\delphi13.0\GR32.graphics32\3.1.0\Source');
//
//
//    method := envOptsType.GetMethod('SaveOptions');
//    paramTypes := method.GetParameters;
//    SetLength(params, Length(paramTypes));
//    method.Invoke(FEnvOptions, params);
//
////    sl.SaveToFile('c:\temp\envoptions.txt');
//
//  finally
//    sl.Free;
//  end;
//
//
//end;

//Installed as DPM.Core.Utils.Process.ProcessMessagePump so TProcess keeps the IDE responsive
//while it blocks on the main thread waiting for a package compile to finish. In the CLI the
//hook is never installed, so TProcess does a plain blocking wait.
procedure DPMPumpMessages;
begin
  if Application <> nil then
    Application.ProcessMessages;
end;

function CreateWizard(const BorlandIDEServices : IBorlandIDEServices) : IOTAWizard;
begin
  TSystemUtils.SetIsIDE; //so the core knows it's running in the IDE plugin. needed for design compile
  //Put our own folder (which holds dpm.exe) on the IDE process PATH so msbuild spawned by the IDE
  //to build the project can resolve the copy-local target's bare 'dpm' fallback.
  TSystemUtils.EnsureModuleDirOnPath;
  ProcessMessagePump := DPMPumpMessages; //keep the IDE message loop alive during package compiles
  try
    result := TDPMWizard.Create;
    SplashImage := Vcl.Graphics.TBitmap.Create;
    SplashImage.LoadFromResourceName(HInstance, 'DPMIDELOGO');
    SplashScreenServices.AddPluginBitmap(cWizardTitle, SplashImage.Handle);

    (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(cWizardTitle, cWizardDescription, SplashImage.Handle);

//    DumpEnvironmentOptionNames;

  except
    on E : Exception do
    begin
      MessageDlg('Failed to create Wizard instance ' + #13#10 + e.Message , mtError, [mbOK], 0);
      result := nil;
    end;
  end;

end;

// Remove the wizard when terminating.
procedure TerminateWizard;
var
  Services : IOTAWizardServices;
begin
  ProcessMessagePump := nil; //the IDE is going away - stop TProcess pumping a dying message loop
  Services := BorlandIDEServices as IOTAWizardServices;
  Services.RemoveWizard(wizardIdx);
end;


function InitWizard(const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate : TWizardTerminateProc) : Boolean; stdcall; //FI:O804
var
  wizard : IOTAWizard;
begin
  CoInitializeEx(nil,COINIT_APARTMENTTHREADED);

  try
    wizard := CreateWizard(BorlandIDEServices);
    if wizard <> nil then
    begin
      RegisterProc(wizard);
      Result := True;
      Terminate := TerminateWizard;
    end
    else
      Result := False;

  except
    on E : Exception do
    begin
      MessageDlg('Failed to load wizard. internal failure:' + E.ClassName + ':'
        + E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;


end.

