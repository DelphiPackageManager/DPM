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
  Spring.Collections,
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

  IServiceIndexItem = interface
    ['{BD0F1369-563C-49BE-A84A-905DAAED47A6}']
    function GetResourceUrl : string;
    function GetResourceType : string;

    property ResourceUrl : string read GetResourceUrl;
    property ResourceType : string read GetResourceType;
  end;

  IServiceIndex = interface
    ['{0633BFFC-CAC7-464C-8374-07EC3858E585}']
    function GetItems : IList<IServiceIndexItem>;
    function FindItems(const resourceType : string) : IEnumerable<IServiceIndexItem>;
    //just finds the first one of the type.
    function FindItem(const resourceType : string) : IServiceIndexItem;
    property Items : IList<IServiceIndexItem> read GetItems;
  end;


implementation

end.

