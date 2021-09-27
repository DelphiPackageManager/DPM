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

unit DPM.Core.Sources.Interfaces;

interface

uses
  VSoft.Awaitable,
  DPM.Core.Sources.Types,
  DPM.Core.Options.Push,
  DPM.Core.Options.Sources,
  VSoft.Uri;

type
  ISourcesManager = interface
    ['{8854A223-60C9-4D65-8FFC-37A0AA503103}']
    function AddSource(const options : TSourcesOptions) : boolean;
    function RemoveSource(const options : TSourcesOptions) : boolean;
    function ListSources(const options : TSourcesOptions) : boolean;
    function EnableSource(const options : TSourcesOptions) : boolean;
    function DisableSource(const options : TSourcesOptions) : boolean;
    function UpdateSource(const options : TSourcesOptions) : boolean;
    //TODO : provide some way to re-order the sources when we have a UI.
  end;

  ISourceClient = interface
    ['{DE344763-CFAA-491A-84F0-5E8462D2F6C3}']
    function Push(const pushOptions : TPushOptions; const cancellationToken : ICancellationToken) : boolean;
  end;

  ISourceClientFactory = interface
    ['{02B5206C-6DBF-41C0-AAC6-09E23856C980}']
    function CreateClient(const uri : IUri) : ISourceClient;
  end;


implementation

end.

