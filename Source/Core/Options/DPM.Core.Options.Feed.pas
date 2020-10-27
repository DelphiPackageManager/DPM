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

unit DPM.Core.Options.Feed;

interface


uses
  DPM.Core.Types,
  DPM.Core.Options.Search;

//NOTE : This is just for testing package feeds from the command line.

type
  TFeedOptions = class(TSearchOptions)
  private
    class var
      FDefault : TFeedOptions;
  public
    class constructor CreateDefault;
    class property Default : TFeedOptions read FDefault;
    constructor Create; override;

  end;

implementation

{ TFeedOptions }

constructor TFeedOptions.Create;
begin
  inherited;

end;

class constructor TFeedOptions.CreateDefault;
begin
  FDefault := TFeedOptions.Create;
end;

end.

