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


unit DPM.Console.Options;

interface

uses
  DPM.Core.Types,
  DPM.Console.Types;

  //NOTE : obsolete - will be removed - changing to indivual options per command in the core.


//type
//  TCommandOptions = class
//  public
//    class var
//      Command : TDPMCommand;
//  end;
//
//  TCacheOptions = class
//  public
//    class var
//      PackageId : string;
//      PackageVersion : string;
//      Source : string;
//      Platform : TDPMPlatform;
//      CompilerVersion : TCompilerVersion;
//  end;
//
//
//
//  TConfigOptions = class
//  public
//    class var
//      UseSymLinks : boolean; //use symlinks to package cache when installing package
//      SetValues : string;
//  end;
//
//  TDeleteOptions = class
//  public
//
//  end;
//
type
  THelpOptions = class
  public
    class var
      HelpCommand : TDPMCommand;
  end;
//
//  TListOptions = class
//  public
//    class var
//      SearchTerm : string;
//      Source : string; //source to list package for
//  end;
//
//  TUpdateOptions = class
//  public
//
//  end;
//
//
//
//
//  TPushOptions = class
//  public
//
//  end;
//
//  TSpecOptions = class
//  public
//    class var
//      SpecFile : string;
//      FromProject : string;
//      PackagesFile : string;
//  end;


implementation


initialization
//  //default to help if no other command specified;
//  TCommandOptions.Command := TDPMCommand.Help;
//  //default to help if not other command specified after help.
  THelpOptions.HelpCommand := TDPMCommand.None;
//
//  //default is use symlinks, saves disk space.
//  TConfigOptions.UseSymLinks := true;
//


end.
