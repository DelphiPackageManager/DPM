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

unit DPM.Core.Options.MCP;

interface

uses
  DPM.Core.Types,
  DPM.Core.Options.Base;

type
  ///<summary> Options for the 'mcp' command - a read only MCP server over stdio. </summary>
  TMCPOptions = class(TOptionsBase)
  private
    FCompilerVersion : TCompilerVersion;
    FProjectPath : string;
    FSources : string;
    FLogFile : string;
    class var
      FDefault : TMCPOptions;
  public
    class constructor CreateDefault;
    class property Default : TMCPOptions read FDefault;
    constructor Create; override;

    ///<summary>
    ///  Session default compiler. Tools may override it per call, and every tool result says
    ///  which compiler it actually used, so an inferred value can never mislead silently.
    ///</summary>
    property CompilerVersion : TCompilerVersion read FCompilerVersion write FCompilerVersion;
    ///<summary> Session default project. Falls back to the single .dproj in the working directory. </summary>
    property ProjectPath : string read FProjectPath write FProjectPath;
    property Sources : string read FSources write FSources;
    ///<summary>
    ///  When set, every MCP frame is appended to this file in both directions.
    ///  Diagnostics only - stdout cannot be used for it, since stdout is the protocol.
    ///</summary>
    property LogFile : string read FLogFile write FLogFile;
  end;

implementation

{ TMCPOptions }

constructor TMCPOptions.Create;
begin
  inherited;
  FCompilerVersion := TCompilerVersion.UnknownVersion;
end;

class constructor TMCPOptions.CreateDefault;
begin
  FDefault := TMCPOptions.Create;
end;

end.
