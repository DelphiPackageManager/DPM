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

unit DPM.Core.Packaging.Archive.Writer;

interface

uses
  System.Classes,
  System.Zip,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Packaging.Archive;

type
  TPackageArchiveWriter = class(TInterfacedObject,IPackageArchiveWriter)
  private
    FFileName   : string;
    FZipFile    : TZipFile;
    FLastError  : string;
    FBasePath   : string;
    FVersion    : TPackageVersion;
    FLogger     : ILogger;
  protected
    function AddFile(const filePath: string): Boolean;overload;
    function AddFile(const fileName : string; const archiveFileName : string) : boolean;overload;
    function AddFiles(const files: System.TArray<System.string>): Boolean;
    function RemoveFile(const filePath: string): Boolean;
    function WriteMetaDataFile(const stream: TStream): Boolean;overload;
    function WriteMetaDataFile(const fileName : string) : boolean;overload;
    function Exists: Boolean;
    function GetArchiveName: string;
    function GetArchivePath: string;
    function GetLastErrorString: string;
    function IsArchive: Boolean;
    function Open(const fileName : string) : Boolean;
    procedure Close;
    procedure SetBasePath(const path : string);
    function GetPackageVersion: TPackageVersion;

  public
    constructor Create(const logger : ILogger);overload;
    destructor Destroy;override;
  end;


implementation

uses
  DPM.Core.Constants,
  System.SysUtils,
  System.IOUtils;

{ TPackageArchiveWriter }

function TPackageArchiveWriter.AddFile(const filePath: string): Boolean;
var
  archiveFileName : string;
begin
  archiveFileName := ExtractRelativePath(FBasePath,filePath);
  FZipFile.Add(filePath,archiveFileName);
  result := true;
end;

function TPackageArchiveWriter.AddFile(const fileName, archiveFileName: string): boolean;
begin
  FZipFile.Add(fileName,archiveFileName);
  result := true;
end;

function TPackageArchiveWriter.AddFiles(const files: System.TArray<System.string>): Boolean;
var
  f : string;
begin
  for f in files do
  begin
    if not AddFile(f) then
      Exit(false);
  end;
  result := true;
end;

procedure TPackageArchiveWriter.Close;
begin
  if FZipFile <> nil then
  begin
    FZipFile.Close;
    FreeAndNil(FZipFile);
  end;
end;

constructor TPackageArchiveWriter.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;


end;

destructor TPackageArchiveWriter.Destroy;
begin
  if FZipFile <> nil then
    FZipFile.Free;
  inherited;
end;

function TPackageArchiveWriter.Exists: Boolean;
begin
  result := (FZipFile <> nil) or TFile.Exists(FFileName);
end;

function TPackageArchiveWriter.GetArchiveName: string;
begin
  result := TPath.GetFileName(FFileName);
end;

function TPackageArchiveWriter.GetArchivePath: string;
begin
//  if FFileStream <> nil then
//    result := 'c:\'
//  else
    result := TPath.GetDirectoryName(FFileName);

end;

function TPackageArchiveWriter.GetLastErrorString: string;
begin
  result := FLastError;
end;

function TPackageArchiveWriter.GetPackageVersion: TPackageVersion;
begin
  result := FVersion;
end;

function TPackageArchiveWriter.IsArchive: Boolean;
begin
  result := true;
end;

function TPackageArchiveWriter.Open(const fileName : string) : Boolean;
begin
  FFileName := fileName;
  FVersion := TPackageVersion.Empty;

  result := false;
  if FZipFile <> nil then
    exit(true); //already open;

  FZipFile := TZipFile.Create;
  try
    FZipFile.Open(FFileName,TZipMode.zmWrite);
    result := true;
  except
    on e : Exception do
    begin
      FreeAndNil(FZipFile);
      FLogger.Error('Error opening package file : ' + e.Message);
      FLastError := e.ToString;
    end;
  end;

end;

function TPackageArchiveWriter.RemoveFile(const filePath: string): Boolean;
begin
  raise ENotImplemented.Create('RemoveFile not implemented');
end;

procedure TPackageArchiveWriter.SetBasePath(const path: string);
begin
  FBasePath := path;
end;

function TPackageArchiveWriter.WriteMetaDataFile(const fileName: string): boolean;
begin
  FZipFile.Add(fileName, cPackageMetaFile);
  result := true;
end;

function TPackageArchiveWriter.WriteMetaDataFile(const stream: TStream): Boolean;
begin
  FZipFile.Add(stream,cPackageMetaFile);
  result := true;
end;


end.
