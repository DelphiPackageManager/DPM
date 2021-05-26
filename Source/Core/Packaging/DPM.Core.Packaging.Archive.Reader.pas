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

unit DPM.Core.Packaging.Archive.Reader;

interface

uses
  System.Classes,
  System.Zip,
  DPM.Core.Types,
  DPM.Core.Packaging.Archive;

  //not used

//type
//  TZipFileArchiveReader = class(TInterfacedObject, IPackageArchiveReader)
//  private
//    FFileName : string;
//    FZipFile : TZipFile;
//    FPackageMetaFile : string;
//    FLastError : string;
//    FVersion : TPackageVersion;
//  protected
//    function Exists : Boolean;
//    function GetArchiveName : string;
//    function GetArchivePath : string;
//    function IsArchive : Boolean;
//    function Open(const fileName : string) : Boolean;
//    procedure Close;
//
//    function GetLastErrorString : string;
//    function GetPackageVersion : TPackageVersion;
//
//    function ReadFileNames : TArray<string>;
//    function ReadMetaDataFile(const stream : TStream) : Boolean;
//    function ExtractFileTo(const fileName : string; const destFileName : string) : boolean;
//    function ExtractTo(const path : string) : boolean;
//
//  public
//    constructor Create;
//    destructor Destroy; override;
//
//  end;
//
//  TFolderArchiveReader = class(TInterfacedObject, IPackageArchiveReader)
//  private
//    FFolderName : string;
//    FPackageMetaFile : string;
//    FLastError : string;
//    FVersion : TPackageVersion;
//  protected
//    function Exists : Boolean;
//    function GetArchiveName : string;
//    function GetArchivePath : string;
//    function IsArchive : Boolean;
//    function Open(const fileName : string) : Boolean;
//    procedure Close;
//    function GetLastErrorString : string;
//    function GetPackageVersion : TPackageVersion;
//
//    function ReadFileNames : System.TArray<System.string>;
//    function ReadMetaDataFile(const stream : TStream) : Boolean;
//    function ExtractFileTo(const fileName : string; const destFileName : string) : boolean;
//    function ExtractTo(const path : string) : Boolean;
//
//  public
//    constructor Create;
//    destructor Destroy; override;
//
//  end;
//


implementation

uses
  System.IOUtils,
  System.SysUtils,
  DPM.Core.Constants;

//{ TZieFileArchiveReader }
//
//procedure TZipFileArchiveReader.Close;
//begin
//  if FZipFile <> nil then
//  begin
//    FZipFile.Close;
//    FreeAndNil(FZipFile);
//  end;
//end;
//
//constructor TZipFileArchiveReader.Create;
//begin
//  inherited;
//end;
//
//destructor TZipFileArchiveReader.Destroy;
//begin
//  if FZipFile <> nil then
//  begin
//    FZipFile.Close;
//    FZipFile.Free;
//  end;
//
//  inherited;
//end;
//
//function TZipFileArchiveReader.Exists : Boolean;
//begin
//  result := TFile.Exists(FFileName);
//end;
//
//function TZipFileArchiveReader.ExtractFileTo(const fileName, destFileName : string) : boolean;
//var
//  sPath : string;
//begin
//  result := false;
//  if FZipFile = nil then
//  begin
//    FLastError := 'Archive not open';
//    exit;
//  end;
//  try
//    sPath := TPath.GetDirectoryName(destFileName);
//    FZipFile.Extract(fileName, sPath, true);
//    result := true;
//  except
//    on e : Exception do
//      FLastError := e.ToString;
//  end;
//end;
//
//function TZipFileArchiveReader.ExtractTo(const path : string) : boolean;
//begin
//  result := false;
//  if FZipFile = nil then
//  begin
//    FLastError := 'Archive not open';
//    exit;
//  end;
//
//  try
//    //TODO : This isn't overwriting!!!
//
//    FZipFile.ExtractAll(path);
//    result := true;
//  except
//    on e : Exception do
//      FLastError := e.ToString;
//  end;
//
//end;
//
//function TZipFileArchiveReader.GetArchiveName : string;
//begin
//  result := TPath.GetFileName(FFileName);
//end;
//
//function TZipFileArchiveReader.GetArchivePath : string;
//begin
//  result := TPath.GetDirectoryName(FFileName)
//end;
//
//function TZipFileArchiveReader.GetLastErrorString : string;
//begin
//  result := FLastError;
//end;
//
//function TZipFileArchiveReader.GetPackageVersion : TPackageVersion;
//begin
//  result := FVersion;
//end;
//
//function TZipFileArchiveReader.IsArchive : Boolean;
//begin
//  result := TPath.GetExtension(FFileName) = cPackageFileExt;
//end;
//
//function TZipFileArchiveReader.Open(const fileName : string) : Boolean;
//var
//  sFileName : string;
//  i : integer;
//begin
//  FFileName := fileName;
//  FPackageMetaFile := cPackageMetaFile;
//  sFileName := ExtractFileName(fileName);
//  sFileName := ChangeFileExt(sFileName, '');
//  i := Pos('-', sFileName);
//  if i <> -1 then
//    Delete(sFileName, 1, i);
//
//
//  FVersion := TPackageVersion.Parse(sFileName);
//
//  result := false;
//  if FZipFile <> nil then
//    exit(true); //already open;
//
//  FZipFile := TZipFile.Create;
//  try
//    FZipFile.Open(FFileName, TZipMode.zmRead);
//    result := true;
//  except
//    on e : Exception do
//    begin
//      FreeAndNil(FZipFile);
//      FLastError := e.ToString;
//    end;
//  end;
//end;
//
//function TZipFileArchiveReader.ReadFileNames : System.TArray<System.string>;
//begin
//  if FZipFile = nil then
//  begin
//    SetLength(result, 0);
//    FLastError := 'Archive not open!'; //todo : resourcestring!
//  end;
//  result := FZipFile.FileNames;
//end;
//
//function TZipFileArchiveReader.ReadMetaDataFile(const stream : TStream) : Boolean;
//var
//  localheader : TZipHeader;
//  localStream : TStream;
//begin
//  result := false;
//  if FZipFile = nil then
//  begin
//    FLastError := 'Archive not open!';
//    exit;
//  end;
//  try
//    FZipFile.Read(cPackageMetaFile, localstream, localheader);
//    try
//      stream.CopyFrom(localStream, localStream.Size);
//      result := true;
//    finally
//      localStream.Free;
//    end;
//  except
//    on e : Exception do
//      FLastError := e.ToString;
//  end;
//end;
//
//{ TFolderArchiveReader }
//
//procedure TFolderArchiveReader.Close;
//begin
//
//end;
//
//constructor TFolderArchiveReader.Create();
//begin
//end;
//
//destructor TFolderArchiveReader.Destroy;
//begin
//
//  inherited;
//end;
//
//function TFolderArchiveReader.Exists : Boolean;
//begin
//  result := TDirectory.Exists(FFolderName);
//end;
//
//function TFolderArchiveReader.ExtractFileTo(const fileName, destFileName : string) : boolean;
//begin
//  result := False;
//  try
//    TFile.Copy(fileName, destFileName);
//    result := true;
//  except
//    on e : Exception do
//      FLastError := e.ToString;
//  end;
//end;
//
//function TFolderArchiveReader.ExtractTo(const path : string) : Boolean;
//begin
//  result := False;
//  try
//    TDirectory.Copy(FFolderName, path);
//    result := true;
//  except
//    on e : Exception do
//      FLastError := e.ToString;
//  end;
//
//end;
//
//function TFolderArchiveReader.GetArchiveName : string;
//begin
//  result := FFolderName;
//end;
//
//function TFolderArchiveReader.GetArchivePath : string;
//begin
//  result := FFolderName;
//end;
//
//function TFolderArchiveReader.GetLastErrorString : string;
//begin
//  result := FLastError;
//end;
//
//function TFolderArchiveReader.GetPackageVersion : TPackageVersion;
//begin
//  result := FVersion;
//end;
//
//function TFolderArchiveReader.IsArchive : Boolean;
//begin
//  result := false;
//end;
//
//function TFolderArchiveReader.Open(const fileName : string) : Boolean;
//begin
//  FFolderName := fileName;
//  FPackageMetaFile := TPath.Combine(FFolderName, cPackageMetaFile);
//  FVersion := TPackageVersion.Parse(FFolderName); // TODO : This would need to be just the last part of the folder.
//
//  //Nothing to do here really, but we'll do a sanity check!
//  result := TDirectory.Exists(FFolderName) and TFile.Exists(FPackageMetaFile);
//end;
//
//function TFolderArchiveReader.ReadFileNames : TArray<string>;
//begin
//  result := TArray <string> (TDirectory.GetFiles(FFolderName, '*.*', TSearchOption.soAllDirectories));
//end;
//
//function TFolderArchiveReader.ReadMetaDataFile(const stream : TStream) : Boolean;
//var
//  fs : TFileStream;
//begin
//  result := false;
//  try
//    fs := TFile.OpenRead(FPackageMetaFile);
//    try
//      stream.CopyFrom(fs, fs.Size);
//      stream.Seek(0, soBeginning);
//      result := true;
//    finally
//      fs.Free;
//    end;
//  except
//    on e : Exception do
//      FLastError := e.Message;
//  end;
//end;


end.

