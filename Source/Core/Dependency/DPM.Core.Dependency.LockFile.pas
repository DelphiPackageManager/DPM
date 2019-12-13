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

unit DPM.Core.Dependency.LockFile;

interface
uses
  Spring.Collections,
  System.Classes,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces;

type
  TLockFileReader = class(TInterfacedObject, ILockFileReader)
  private
    class var
      FCurrentVersion : TPackageVersion;
  private
    FLogger : ILogger;
  protected
    function ParseLockFile(const strings : TStringList) : IGraphNode;
    function TryLoadFromString(const value : string; out lockFile : ILockFile) : boolean; //only for testing
    function TryLoadFromFile(const fileName : string; out lockFile : ILockFile) : boolean;
    function CreateNew(const fileName : string) : ILockFile;
    class constructor Create;
  public
    constructor Create(const logger : ILogger);
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Strings,
  DPM.Core.Constants,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Graph;


type
  TLockFile = class(TInterfacedObject, ILockFile)
  private
    FGraph : IGraphNode;
    FFileName : string;
  protected
    function IsValid(const topLevel : IEnumerable<IPackageDependency>) : boolean;
    function GetFileName : string;
    function GetGraph : IGraphNode;
    function CommitToFile(const newFileName : string = '') : boolean;
    function CommitToStream(const stream : TStream) : boolean;
  public
    constructor Create(const fileName : string; const graph : IGraphNode);
  end;


{ TLockFileLoader }

const
  cLockFileHeader = '# DPM LockFile ';

function SpaceLengthAtStart(const line : string) : integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(line) do
  begin
    if line[i] = ' ' then
      inc(result)
    else
      exit;
  end;
end;


{ TLockFile }


function TLockFile.CommitToFile(const newFileName : string = ''): boolean;
begin
   result := false;
end;

function TLockFile.CommitToStream(const stream: TStream): boolean;
begin
   result := false;
end;

constructor TLockFile.Create(const fileName: string; const graph: IGraphNode);
begin
  FFileName := fileName;
  FGraph := graph;
end;

function TLockFile.GetFileName: string;
begin
  result := FFileName;
end;

function TLockFile.GetGraph: IGraphNode;
begin
  result := FGraph;
end;

function TLockFile.IsValid(const topLevel: IEnumerable<IPackageDependency>): Boolean;
begin
  result := false;
end;


{ TLockFileReader }

constructor TLockFileReader.Create(const logger: ILogger);
begin
  FLogger := logger;
end;

class constructor TLockFileReader.Create;
begin
  FCurrentVersion := TPackageVersion.Parse(cLockFileVersion);
end;

function TLockFileReader.CreateNew(const fileName: string): ILockFile;
var
  graph : IGraphNode;
begin
  graph := TGraphNode.CreateRoot;
  graph.State := TGraphNodeState.Unknown;
  result := TLockFile.Create(fileName,graph);
end;

function TLockFileReader.ParseLockFile(const strings: TStringList): IGraphNode;
var
  lineNo : integer;
  line : string;
  currentLevel : integer;
  level : integer;
  currentNode : IGraphNode;
  currentParent : IGraphNode;
  diff : integer;

  procedure ValidateHeader(value : string);
  var
    version : TPackageVersion;
    error : string;
  begin
    if not TStringUtils.StartsWith(line, cLockFileHeader) then
      raise Exception.Create('Not a valid Lock file.');
    Delete(value, 1, Length(cLockFileHeader));
    if not TPackageVersion.TryParseWithError(value, version, error) then
      raise Exception.Create('Lock file version is not a valid semantic version : ' + error);
    if version <> FCurrentVersion then
      raise Exception.Create('Lock file version is out of date');
  end;

  function AddNode(const parentNode : IGraphNode; const value : string) : IGraphNode;
  var
    parts : TArray<string>;
    id : string;
    version : TPackageVersion;
    error : string;
    range : TVersionRange;
  begin
    parts := TStringUtils.SplitStr(value, ' ',TSplitStringOptions.ExcludeEmpty);
    if length(parts) < 2 then
      raise Exception.Create('Not a valid lock file Line');
    id := parts[0];
    if not TPackageVersion.TryParseWithError(parts[1], version, error) then
      raise Exception.Create('Not a valid lock file Line : ' + error);
    if length(parts) > 2 then
    begin
      if not TVersionRange.TryParseWithError(parts[2],range, error) then
        raise Exception.Create('Not a valid lock file Line : ' + error);
    end
    else
      range := TVersionRange.Empty;


    result := parentNode.AddChildNode(id,version,range, nil)


  end;

begin
  result := TGraphNode.CreateRoot;
  currentLevel := 0;
  currentParent := result;
  currentNode := nil;
  for lineNo := 0 to strings.Count -1  do
  begin
    line := strings[lineNo];
    if lineNo = 0  then
      ValidateHeader(line);//throws

    //skip comment lines
    if TStringUtils.StartsWith(line, '#') then
      continue;

    if line = '' then
      continue;

    level := SpaceLengthAtStart(line);
    if not Result.HasChildren and (level <> 0) then
      raise Exception.Create('Invalid lock file formatting on line [' + IntToStr(lineNo) +']');

    if (level > 0) and (level mod 2 > 0) then
      raise Exception.Create('Invalid lock file formatting on line [' + IntToStr(lineNo) +'] - leading spaces must be even in number.');

    if level = 0 then
     //new topLevel node;
      currentParent := result
    else if level > currentLevel then
      //creating a new child
      currentParent := currentNode
    else if level < currentLevel then
    begin
      diff := currentLevel - level;
      while (diff > 0) and (currentParent <> nil) do
      begin
        currentParent := currentParent.Parent;
        Dec(diff,2);
      end;
    end;
    if currentParent = nil then
      currentParent := result;

    currentLevel := level;

    currentNode := AddNode(currentParent, Trim(line));
  end;

end;

function TLockFileReader.TryLoadFromFile(const fileName: string; out lockFile: ILockFile): boolean;
var
  sList : TStringList;
  graph : IGraphNode;
begin
  result := true;
  try
    if not FileExists(fileName) then
      raise Exception.Create('Lock file not found.' );
    sList := TStringList.Create;
    try
      sList.Delimiter := #0;
      sList.LoadFromFile(fileName);
      graph := ParseLockFile(sList);
      lockFile := TLockFile.Create(fileName,graph);
    finally
      sList.Free;
    end;
  except
    on e : Exception do
    begin
      result := false;
      FLogger.Error('Error loading lockfile from [' + fileName + ']');
      FLogger.Error('   ' + e.Message);
    end;

  end;
end;

function TLockFileReader.TryLoadFromString(const value: string; out lockFile: ILockFile): boolean;
var
  sList : TStringList;
  graph : IGraphNode;
begin
  result := true;
  try
    sList := TStringList.Create;
    try
      sList.Text := value;
      graph := ParseLockFile(sList);
      lockFile := TLockFile.Create('',graph);
    finally
      sList.Free;
    end;
  except
    on e : Exception do
    begin
      result := false;
      FLogger.Error('Error loading lockfile - ' + e.Message);
    end;

  end;
end;

end.
