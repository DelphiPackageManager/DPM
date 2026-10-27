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

unit DPM.Core.Spec.CopyLocalEntry;

interface
uses
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  //A first-class copyLocal entry. Unlike a source entry (which can only reference a file that
  //exists at pack time), this matches files in the package cache at build/copylocal time -
  //including artifacts produced during install (e.g. a runtime .bpl in bpl\{platform}). The
  //src glob may use the $platform$ token, which 'dpm copylocal' substitutes with the build
  //platform's folder name before expanding.
  TSpecCopyLocalEntry = class(TSpecNode, ISpecCopyLocalEntry)
  private
    FSource : string;
    FPlatforms : TDPMPlatforms;
  protected
    function GetSource : string;
    function GetPlatforms : TDPMPlatforms;

    procedure SetSource(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function Clone : ISpecCopyLocalEntry;
    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;

    property Source : string read GetSource write SetSource;
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
  public
    constructor CreateClone(const logger : ILogger; const src : string; const platforms : TDPMPlatforms); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Diagnostics;

{ TSpecCopyLocalEntry }

function TSpecCopyLocalEntry.Clone : ISpecCopyLocalEntry;
begin
  result := TSpecCopyLocalEntry.CreateClone(logger, FSource, FPlatforms);
end;

constructor TSpecCopyLocalEntry.CreateClone(const logger : ILogger; const src : string; const platforms : TDPMPlatforms);
begin
  inherited Create(logger);
  FSource := src;
  FPlatforms := platforms;
end;

function TSpecCopyLocalEntry.GetPlatforms : TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TSpecCopyLocalEntry.GetSource : string;
begin
  result := FSource;
end;

function TSpecCopyLocalEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  platformsSeq : IYAMLSequence;
  i : integer;
  platform : TDPMPlatform;
  sPlatform : string;
begin
  result := true;
  FSource := yamlObject.S['src'];
  if FSource = '' then
  begin
    Logger.Error(TErrorCode.E6003_SpecElementMissing, 'A copyLocal entry is missing the required [src].');
    result := false;
  end;

  FPlatforms := [];
  if not SequenceOf(yamlObject, 'platforms', 'the copyLocal entry [' + FSource + ']', platformsSeq) then
    result := false;
  if platformsSeq.Count > 0 then
  begin
    for i := 0 to platformsSeq.Count -1 do
    begin
      sPlatform := platformsSeq.S[i];
      platform := StringToDPMPlatform(sPlatform);
      if platform <> TDPMPlatform.UnknownPlatform then
        FPlatforms := FPlatforms + [platform]
      else
      begin
        //not dropped - no platforms means all of them, so leaving it out would widen the entry.
        Logger.Error(TErrorCode.E6004_SpecValueInvalid, 'The platform [' + sPlatform + '] of the copyLocal entry [' + FSource + '] is not a platform DPM knows.');
        result := false;
      end;
    end;
  end;
  //A legacy 'mode' key (always/runtimeOnly) is no longer used - runtime bpls are auto-copied and
  //explicit entries are always copied - so it is simply ignored here.
end;

procedure TSpecCopyLocalEntry.SetPlatforms(const value : TDPMPlatforms);
begin
  FPlatforms := value;
end;

procedure TSpecCopyLocalEntry.SetSource(const value : string);
begin
  FSource := value;
end;

procedure TSpecCopyLocalEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  mapping : IYAMLMapping;
  platformsSeq : IYAMLSequence;
  platform : TDPMPlatform;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['src'] := FSource;
  if FPlatforms <> [] then
  begin
    platformsSeq := mapping.A['platforms'];
    for platform in FPlatforms do
      platformsSeq.AddValue(DPMPlatformToString(platform));
  end;
end;

end.
