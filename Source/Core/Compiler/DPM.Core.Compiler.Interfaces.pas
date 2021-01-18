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

unit DPM.Core.Compiler.Interfaces;

interface

uses
  System.Classes,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types;

{$SCOPEDENUMS ON}

type
  TCompilerVerbosity = (Quiet, Minimal, Normal, Detailed, Diagnostic);

  ICompiler = interface
    ['{4A56BA53-6ACD-4A5D-8D55-B921D6CDC8A0}']
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;

    function GetConfiguration : string;
    procedure SetConfiguration(const value : string);


    function GetSearchPaths : IList<string>;
    procedure SetSearchPaths(const value : IList<string> );

    function GetBPLOutput : string;
    procedure SetBPLOutput(const value : string);

    function GetLibOutput : string;
    procedure SetLibOutput(const value : string);

    function GetVerbosity : TCompilerVerbosity;
    procedure SetVerbosity(const value : TCompilerVerbosity);

    function GetCompilerOutput : TStrings;

    function BuildProject(const cancellationToken : ICancellationToken; const projectFile : string; const configName : string; const forDesign : boolean = false) : boolean;

    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Configuration : string read GetConfiguration write SetConfiguration;
    property Platform : TDPMPlatform read GetPlatform;
    property Verbosity : TCompilerVerbosity read GetVerbosity write SetVerbosity;

    property BPLOutputDir : string read GetBPLOutput write SetBPLOutput;
    property LibOutputDir : string read GetLibOutput write SetLibOutput;
    property CompilerOutput : TStrings read GetCompilerOutput;
  end;

  //inject
  ICompilerEnvironmentProvider = interface
    ['{54814318-551F-4F53-B0FB-66AC0E430DB7}']
    function GetRsVarsFilePath(const compilerVersion : TCompilerVersion) : string;
  end;

  //inject
  ICompilerFactory = interface
    ['{3405435B-5D3A-409A-AFB7-FEFA0EA07060}']
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler;
  end;

implementation

end.


