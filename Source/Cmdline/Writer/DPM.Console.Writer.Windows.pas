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

unit DPM.Console.Writer.Windows;

interface

uses
  DPM.Console.Writer;

type
  //TODO : Make thread safe!
  TWindowsConsole = class(TConsoleBase)
  private
    FDefaultForeground : Word;
    FDefaultBackground : Word;
    FLastForeground : TConsoleColor;
    FLastBackground : TConsoleColor;
    FStdOut : THandle;
    function GetForegroundColourCode(const cc: TConsoleColor): Word;
    function GetBackgroundColourCode(const cc: TConsoleColor): Word;

    function ConsoleAttributesToConsoleColor(const value : word; const background : boolean = false) : TConsoleColor;
  protected
    function GetConsoleWidth : Integer;override;
    procedure InternalWriteLn(const s : String); override;
    procedure InternalWrite(const s : String); override;

    function  GetCurrentForegroundColor : TConsoleColor; override;
    function  GetCurrentBackgroundColor : TConsoleColor; override;
    procedure SetForegroundColor(const foreground : TConsoleColor);override;
    procedure SetColour(const foreground: TConsoleColor; const background: TConsoleColor = ccDefault); override;
  public
    constructor Create;override;
    destructor Destroy; override;
  end;


implementation

uses
  WinAPI.Windows,
  DPM.Core.Utils.Strings;

constructor TWindowsConsole.Create;
var
  dummy : Cardinal;
  consoleInfo : _CONSOLE_SCREEN_BUFFER_INFO;
begin
  inherited;

  FStdOut := GetStdHandle(STD_OUTPUT_HANDLE);

  if not GetConsoleMode(FStdOut, dummy) then // Not a console handle
    Self.RedirectedStdOut := True;

  Self.ConsoleWidth := GetConsoleWidth;

  //Save the current console colour settings so we can restore them:
  if GetConsoleScreenBufferInfo(FStdOut, consoleInfo) then
  begin
    FDefaultForeground := consoleInfo.wAttributes and (FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
    FDefaultBackground := consoleInfo.wAttributes and (BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY);
  end
  else
  begin
    FDefaultForeground := GetForegroundColourCode(ccWhite);
    FDefaultBackground := GetBackgroundColourCode(ccBlack);
  end;
//  FLastForeground := ccDarkYellow; // Just to ensure the first colour change goes through
//  FLastBackground := ccDarkYellow;

  FLastForeground := ConsoleAttributesToConsoleColor(FDefaultForeground);
  FLastBackground := ConsoleAttributesToConsoleColor(FDefaultBackground);

end;

function TWindowsConsole.ConsoleAttributesToConsoleColor(const value : Word; const background : boolean = false) : TConsoleColor;
var
  intensity : boolean;
  red : boolean;
  blue : boolean;
  green : boolean;
begin
  if background then
  begin
    red   := (value and BACKGROUND_RED) > 0;
    green := (value and BACKGROUND_GREEN) > 0;
    blue := (value and BACKGROUND_BLUE) > 0;
    intensity := (value and BACKGROUND_INTENSITY) > 0;
    result := TConsoleColor.ccBlack;

  end
  else
  begin
    red   := (value and FOREGROUND_RED) > 0;
    green := (value and FOREGROUND_GREEN) > 0;
    blue := (value and FOREGROUND_BLUE) > 0;
    intensity := (value and FOREGROUND_INTENSITY) > 0;
    result := TConsoleColor.ccWhite;
  end;

  //red only
  if red then
  begin
    //test with blue and green
    if blue and green then
      result := ccWhite
    else if blue then
      result := ccDarkPurple
    else if green then
      result := ccDarkYellow
    else
      result := ccDarkRed;
  end
  else if blue then
  begin
    //test with green only since red&blue is tested under red
    if green then
      result := ccDarkAqua
    else
      result := ccDarkBlue;

  end
  else if green then
  begin
    result := ccDarkGreen;
  end
  else if not (red or blue or green) then
  begin
    if intensity then
      result := ccGrey
    else
      result := ccBlack;
  end;

  if intensity then
  begin
    case result of
      ccDarkRed: result := ccBrightRed;
      ccDarkBlue: result := ccBrightBlue;
      ccDarkGreen: result := ccBrightGreen;
      ccDarkYellow: result := ccBrightYellow;
      ccDarkAqua: result := ccBrightAqua;
      ccDarkPurple: result := ccBrightPurple;
      ccBlack: result := ccGrey ;
      ccWhite: result := ccBrightWhite;
    else
      //fi
    end;
  end;

end;



procedure TWindowsConsole.SetColour(const foreground, background: TConsoleColor);
begin
  if (FLastForeground <> foreground) or (FLastBackground <> background) then
  begin
    SetConsoleTextAttribute(FStdOut,
       GetForegroundColourCode(foreground) or
       GetBackgroundColourCode(background));

    FLastForeground := foreground;
    FLastBackground := background;
  end;
end;

procedure TWindowsConsole.SetForegroundColor(const foreground: TConsoleColor);
begin
  SetColour(foreground,FLastBackground);
end;

function TWindowsConsole.GetForegroundColourCode(const cc : TConsoleColor) : Word;
begin
  result := 0;
  case cc of
    ccDefault       : result := FDefaultForeground;
    ccBrightRed     : result := FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkRed       : result := FOREGROUND_RED;
    ccBrightBlue    : result := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    ccDarkBlue      : result := FOREGROUND_BLUE;
    ccBrightGreen   : result := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    ccDarkGreen     : result := FOREGROUND_GREEN;
    ccBrightYellow  : result := FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkYellow    : result := FOREGROUND_GREEN or FOREGROUND_RED;
    ccBrightAqua    : result := FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    ccDarkAqua      : result := FOREGROUND_GREEN or FOREGROUND_BLUE;
    ccBrightPurple  : result := FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkPurple    : result := FOREGROUND_BLUE or FOREGROUND_RED;
    ccGrey          : result := FOREGROUND_INTENSITY;
    ccBlack         : result := 0;
    ccBrightWhite   : result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccWhite         : result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
  end;
end;

procedure TWindowsConsole.InternalWrite(const s: String);
var
  output : string;
  dummy : Cardinal;
begin
  //Add the indenting.
  output := TStringUtils.PadString(s, length(s)+ Self.CurrentIndentLevel, True, ' ');
  if Self.RedirectedStdOut then
    System.Write(output)
  else
    WriteConsoleW(FStdOut, PWideChar(output), Length(output), dummy, nil);
end;

procedure TWindowsConsole.InternalWriteLn(const s: String);
var
  output : string;
  dummy : Cardinal;
begin
  //Add the indenting.
  output := TStringUtils.PadString(s, length(s)+ Self.CurrentIndentLevel, True, ' ');

  //If we are already going to wrap around to the next line. No need to add CRLF
  if Length(output) < ConsoleWidth then
    output := output + #13#10;

  if Self.RedirectedStdOut then
    System.Write(output)
  else
    WriteConsoleW(FStdOut, PWideChar(output), Length(output), dummy, nil);
end;

destructor TWindowsConsole.Destroy;
begin
  SetColour(ccDefault); // Restore default console colours
  inherited;
end;

function TWindowsConsole.GetBackgroundColourCode(const cc : TConsoleColor) : Word;
begin
  result := 0;
  case cc of
    ccDefault       : result := FDefaultBackground;
    ccBrightRed     : result := BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkRed       : result := BACKGROUND_RED;
    ccBrightBlue    : result := BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    ccDarkBlue      : result := BACKGROUND_BLUE;
    ccBrightGreen   : result := BACKGROUND_GREEN or BACKGROUND_INTENSITY;
    ccDarkGreen     : result := BACKGROUND_GREEN;
    ccBrightYellow  : result := BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkYellow    : result := BACKGROUND_GREEN or BACKGROUND_RED;
    ccBrightAqua    : result := BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    ccDarkAqua      : result := BACKGROUND_GREEN or BACKGROUND_BLUE;
    ccBrightPurple  : result := BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkPurple    : result := BACKGROUND_BLUE or BACKGROUND_RED;
    ccGrey          : result := BACKGROUND_INTENSITY;
    ccBlack         : result := 0;
    ccBrightWhite   : result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccWhite         : result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED;
  end;
end;

function TWindowsConsole.GetConsoleWidth: Integer;
var
  info : CONSOLE_SCREEN_BUFFER_INFO;
begin
  result := High(Integer); // Default is unlimited width
  if GetConsoleScreenBufferInfo(FStdOut, info) then
    result := info.dwSize.X;
end;

function TWindowsConsole.GetCurrentBackgroundColor: TConsoleColor;
begin
  result := FLastBackground;
end;

function TWindowsConsole.GetCurrentForegroundColor: TConsoleColor;
begin
  result := FLastForeground;
end;

end.
