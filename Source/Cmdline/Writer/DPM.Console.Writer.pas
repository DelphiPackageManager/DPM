{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.Console.Writer;

interface

uses
  System.Classes;
//TODO : Split this out to another project and share it with DUnitX!

type
 TConsoleColor = (ccDefault, ccBrightRed, ccDarkRed,
                    ccBrightBlue, ccDarkBlue,
                    ccBrightGreen, ccDarkGreen,
                    ccBrightYellow, ccDarkYellow,
                    ccBrightAqua, ccDarkAqua,
                    ccBrightPurple, ccDarkPurple,
                    ccGrey, ccBlack,
                    ccBrightWhite,
                    ccWhite); // the normal colour of text on the console

  IConsoleWriter = interface
  ['{647E2533-7814-4FCE-84E4-A283C371D82F}']
    function GetIsNonInteractive : boolean;
    procedure SetIsNonInteractive(const value : boolean);
    function GetCurrentIndentLevel : Integer;
    procedure SetCurrentIndentLevel(const count: Integer);
    function GetConsoleWidth : integer;
    procedure SetColour(const foreground: TConsoleColor; const background: TConsoleColor = ccDefault);
    procedure WriteLine(const s: String; const foregroundColor : TConsoleColor );overload;
    procedure WriteLine(const s: String);overload;
    procedure WriteLine;overload;
    procedure Write(const s : string);overload;
    procedure Write(const s : string; const foregroundColor : TConsoleColor);overload;

    procedure Indent(const value : integer = 1);
    procedure Outdent(const value : integer = 1);

    property CurrentIndentLevel : Integer read GetCurrentIndentLevel write SetCurrentIndentLevel;
    property IsNonInteractive : boolean read GetIsNonInteractive write SetIsNonInteractive;
    property Width : integer read GetConsoleWidth;

  end;

  TConsoleBase = class(TInterfacedObject,IConsoleWriter)
  private
    FCurrentIndentLevel : integer;
    FConsoleWidth : integer;
    FRedirectedStdOut : boolean;
    FIsNonInteractive : boolean;
  protected
    function GetIsNonInteractive : boolean;
    procedure SetIsNonInteractive(const value : boolean);
    function GetCurrentIndentLevel : Integer;
    procedure SetCurrentIndentLevel(const count: Integer);virtual;
    function InternalBreakupMessage(const s : string): TStringList;
    procedure InternalWriteLn(const s : string); virtual;abstract;
    procedure InternalWrite(const s : string);virtual;abstract;
    procedure Indent(const value : integer = 1);
    procedure Outdent(const value : integer = 1);
    procedure SetColour(const foreground: TConsoleColor; const background: TConsoleColor = ccDefault); virtual;abstract;
    function  GetCurrentForegroundColor : TConsoleColor; virtual;abstract;
    function  GetCurrentBackgroundColor : TConsoleColor; virtual;abstract;
    procedure SetForegroundColor(const foreground : TConsoleColor);virtual;abstract;
    procedure WriteLine(const s: String; const foregroundColor : TConsoleColor );overload;
    procedure WriteLine(const s: String);overload;virtual;
    procedure WriteLine;overload;
    //Deliberate shadow of the System.WriteLn intrinsic. Without it, a WriteLn typo anywhere
    //in this class hierarchy silently binds to System.WriteLn and writes to the RTL Output
    //text file - bypassing the redirect-safe path and blowing up with EInOutError when
    //stdout is a pipe or a file. That exact typo lived here from the initial commit until
    //it was found via 'I/O error 6'. Protected, so descendants are covered too.
    procedure WriteLn(const s : string = '');
    procedure Write(const s : string);overload;virtual;
    procedure Write(const s : string; const foregroundColor : TConsoleColor);overload;virtual;
    function GetConsoleWidth : integer;virtual;abstract;

    property CurrentIndentLevel : Integer read GetCurrentIndentLevel write SetCurrentIndentLevel;
    property ConsoleWidth : integer read FConsoleWidth write FConsoleWidth;
    property RedirectedStdOut : boolean read FRedirectedStdOut write FRedirectedStdOut;
  public
    constructor Create;virtual;
  end;


implementation

const
  DEFAULT_CONSOLE_WIDTH = 80;
  MINIMUM_CONSOLE_WIDTH = 2;

constructor TConsoleBase.Create;
begin
  FConsoleWidth := DEFAULT_CONSOLE_WIDTH;
  FCurrentIndentLevel := 0;
end;

function TConsoleBase.GetCurrentIndentLevel: Integer;
begin
  result := FCurrentIndentLevel;
end;

function TConsoleBase.GetIsNonInteractive: boolean;
begin
  result := FIsNonInteractive;
end;

procedure TConsoleBase.Indent(const value: integer);
begin
  SetCurrentIndentLevel(FCurrentIndentLevel + value);
end;

function TConsoleBase.InternalBreakupMessage(const s: string): TStringList;
var
  line: string;
  offset, width, len : Integer;
  slLines : TStringList;
begin
  result := TStringList.Create;

  //If we are blank string, add on line entry and leave.
  if s = '' then
  begin
    result.Add('');
    Exit;
  end;

  width := FConsoleWidth - FCurrentIndentLevel;
  slLines := TStringList.Create;
  try
    slLines.StrictDelimiter := True;
    slLines.Text := s;

    //Walk through the string list pulling of the console width of characters at a time.
    for line in slLines do
    begin
      len := Length(line);

      if (width > 0) and (len > width) then
      begin
        offset := 1;
        while offset <= len do
        begin
          //Write a line as we have hit the limit of the console.
          result.Add(Copy(line, offset, width));
          Inc(offset, width);
        end;
      end
      else
        //Can write out on a single line
        result.Add(line);
    end;
  finally
    slLines.Free;
  end;
end;

procedure TConsoleBase.Outdent(const value: integer);
begin
  SetCurrentIndentLevel(FCurrentIndentLevel - value);
end;

procedure TConsoleBase.SetCurrentIndentLevel(const count: Integer);
begin
  if Count < 0 then
    FCurrentIndentLevel := 0
  else
  begin
    FCurrentIndentLevel := count;
    if not FRedirectedStdOut then
    begin
      if FCurrentIndentLevel > FConsoleWidth - MINIMUM_CONSOLE_WIDTH then
        FCurrentIndentLevel := FConsoleWidth - MINIMUM_CONSOLE_WIDTH;
    end;
  end;
end;

procedure TConsoleBase.SetIsNonInteractive(const value: boolean);
begin
  FIsNonInteractive := value;
end;

procedure TConsoleBase.Write(const s: string);
var
  slLines : TStringList;
  lineIndex : integer;
begin
  slLines := InternalBreakupMessage(s);
  try
    //Write out all the lines execept the last one.
    for lineIndex := 0 to slLines.Count - 2 do
      InternalWriteLn(slLines[lineIndex]);

    //Now write out the last one without an end of line character.
    if slLines.Count > 0 then
      InternalWrite(slLines[slLines.Count - 1]);
  finally
    slLines.Free;
  end;
end;

procedure TConsoleBase.Write(const s: string; const foregroundColor: TConsoleColor);
var
  currentForeground : TConsoleColor;
  currentBackground : TConsoleColor;
begin
  currentForeground := Self.GetCurrentForegroundColor;
  currentBackground := Self.GetCurrentBackgroundColor;
  try
    SetColour(foregroundColor,currentBackground);
    Write(s);
  finally
    SetColour(currentForeground,currentBackground);
  end;
end;

procedure TConsoleBase.WriteLn(const s : string);
begin
  Self.WriteLine(s);
end;

procedure TConsoleBase.WriteLine;
begin
  //NOTE : this must not use the System.WriteLn intrinsic. That writes to the RTL Output
  //text file, which bypasses the redirect-safe write path in InternalWriteLn and raises
  //EInOutError if InOutRes happens to be set (see the IOResult call in dpm.dpr).
  InternalWriteLn('');
end;

procedure TConsoleBase.WriteLine(const s: String);
var
  slLines : TStringList;
  lineIndex : integer;
begin
  slLines := InternalBreakupMessage(s);
  try
    for lineIndex := 0 to slLines.Count - 1 do
      InternalWriteLn(slLines[lineIndex]);
  finally
    slLines.Free;
  end;
end;


procedure TConsoleBase.WriteLine(const s: String; const foregroundColor: TConsoleColor);
var
  currentForeground : TConsoleColor;
  currentBackground : TConsoleColor;
begin
  currentForeground := Self.GetCurrentForegroundColor;
  currentBackground := Self.GetCurrentBackgroundColor;
  try
    SetColour(foregroundColor, currentBackground);
    //Self.WriteLine, NOT the System.WriteLn intrinsic - see the note in the parameterless
    //overload. Going through WriteLine also gives coloured lines the same indenting and
    //word wrapping every other line gets, which is what Write(s, colour) already does.
    Self.WriteLine(s);
  finally
    SetColour(currentForeground,currentBackground);
  end;

end;

end.
