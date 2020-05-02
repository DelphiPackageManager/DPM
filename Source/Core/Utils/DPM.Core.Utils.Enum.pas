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

unit DPM.Core.Utils.Enum;

interface

type
  TEnumUtils = class
  public
    class function StringToEnum<T: record>(const value : string) : T;
    class function EnumToString<T: record>(const value : T) : string;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Math;


{ TEnumUtils }

class function TEnumUtils.EnumToString<T>(const value: T): string;
var
  P: PTypeInfo;
begin
  P := TypeInfo(T);
  case P^.Kind of
    tkEnumeration:
      case GetTypeData(P)^.OrdType of
        otSByte, otUByte:
          Result := GetEnumName(P, PByte(@Value)^);
        otSWord, otUWord:
          Result := GetEnumName(P, PWord(@Value)^);
        otSLong, otULong:
          Result := GetEnumName(P, PCardinal(@Value)^);
      end;
    else
      raise EArgumentException.CreateFmt('Type %s is not enumeration', [P^.Name]);
  end;
end;

class function TEnumUtils.StringToEnum<T>(const value: string): T;
var
  P: PTypeInfo;
  i : integer;
begin
  P := TypeInfo(T);
  case P^.Kind of
    tkEnumeration:
    begin
      i := GetEnumValue(P, value);
      if InRange(i, p.TypeData.MinValue, p.TypeData.MaxValue) then
      begin
        case Sizeof(T) of
          1: PByte(@Result)^ := GetEnumValue(P, value);
          2: PWord(@Result)^ := GetEnumValue(P, value);
          4: PCardinal(@Result)^ := GetEnumValue(P, value);
        end;
      end
      else //this should probably throw!
      begin
        case Sizeof(T) of
          1: PByte(@Result)^ := p.TypeData.MinValue;
          2: PWord(@Result)^ := p.TypeData.MinValue;
          4: PCardinal(@Result)^ := p.TypeData.MinValue;
        end;
      end;
    end
  else
    raise EArgumentException.CreateFmt('Type %s is not enumeration', [P^.Name]);
  end;

end;

end.
