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

unit DPM.Console.Prompts;

interface

//Interactive console prompt helpers built over VSoft.System.Console. Every
//prompt returns cancelled=True if the user pressed Escape and confirmed they
//want to abort. Callers must check cancelled and unwind without writing anything.

type
  TPromptChoices = array of string;

function PromptLine(const prompt : string; const defaultValue : string; out cancelled : boolean) : string;
function PromptLineRequired(const prompt : string; out cancelled : boolean) : string;
function PromptYesNo(const prompt : string; const defaultYes : boolean; out cancelled : boolean) : boolean;
function PromptChoice(const prompt : string; const choices : TPromptChoices; const defaultIdx : integer; out cancelled : boolean) : integer;

implementation

uses
  System.SysUtils,
  System.Console,
  System.Console.Types;

const
  cCancelPrompt = 'Cancel? (y/N): ';

function ConfirmCancel : boolean;
var
  keyInfo : TConsoleKeyInfo;
  ch : Char;
begin
  Console.WriteLine;
  Console.Write(cCancelPrompt);
  keyInfo := Console.ReadKey(true);
  ch := UpCase(keyInfo.KeyChar);
  Console.WriteLine(ch);
  result := ch = 'Y';
end;

function ReadLineWithEscape(const defaultValue : string; out cancelled : boolean) : string;
var
  keyInfo : TConsoleKeyInfo;
  buf : string;
begin
  cancelled := false;
  buf := '';
  while true do
  begin
    keyInfo := Console.ReadKey(true);
    case keyInfo.Key of
      TConsoleKey.Escape :
        begin
          if ConfirmCancel then
          begin
            cancelled := true;
            result := '';
            exit;
          end;
          //not cancelled - redraw the prompt line continuation
          Console.Write('> ');
          Console.Write(buf);
        end;
      TConsoleKey.Enter :
        begin
          Console.WriteLine;
          if buf = '' then
            result := defaultValue
          else
            result := buf;
          exit;
        end;
      TConsoleKey.Backspace :
        begin
          if Length(buf) > 0 then
          begin
            SetLength(buf, Length(buf) - 1);
            Console.Write(#8 + ' ' + #8);
          end;
        end;
    else
      if (keyInfo.KeyChar <> #0) and (keyInfo.KeyChar >= ' ') then
      begin
        buf := buf + keyInfo.KeyChar;
        Console.Write(keyInfo.KeyChar);
      end;
    end;
  end;
end;

function PromptLine(const prompt : string; const defaultValue : string; out cancelled : boolean) : string;
begin
  if defaultValue <> '' then
    Console.Write(prompt + ' [' + defaultValue + ']: ')
  else
    Console.Write(prompt + ': ');
  result := ReadLineWithEscape(defaultValue, cancelled);
end;

function PromptLineRequired(const prompt : string; out cancelled : boolean) : string;
begin
  cancelled := false;
  while true do
  begin
    Console.Write(prompt + ': ');
    result := ReadLineWithEscape('', cancelled);
    if cancelled then
      exit;
    if Trim(result) <> '' then
      exit;
    Console.WriteLine('  value is required, please enter a value or press Esc to cancel');
  end;
end;

function PromptYesNo(const prompt : string; const defaultYes : boolean; out cancelled : boolean) : boolean;
var
  keyInfo : TConsoleKeyInfo;
  ch : Char;
  hint : string;
begin
  cancelled := false;
  result := defaultYes;
  if defaultYes then
    hint := ' (Y/n): '
  else
    hint := ' (y/N): ';
  while true do
  begin
    Console.Write(prompt + hint);
    keyInfo := Console.ReadKey(true);
    if keyInfo.Key = TConsoleKey.Escape then
    begin
      if ConfirmCancel then
      begin
        cancelled := true;
        exit;
      end;
      continue;
    end;
    if keyInfo.Key = TConsoleKey.Enter then
    begin
      Console.WriteLine;
      exit;
    end;
    ch := UpCase(keyInfo.KeyChar);
    if ch = 'Y' then
    begin
      Console.WriteLine(ch);
      result := true;
      exit;
    end;
    if ch = 'N' then
    begin
      Console.WriteLine(ch);
      result := false;
      exit;
    end;
    Console.WriteLine;
  end;
end;

function PromptChoice(const prompt : string; const choices : TPromptChoices; const defaultIdx : integer; out cancelled : boolean) : integer;
var
  keyInfo : TConsoleKeyInfo;
  i : integer;
  input : string;
  selected : integer;
  defaultHint : string;
begin
  cancelled := false;
  result := defaultIdx;
  if Length(choices) = 0 then
  begin
    result := -1;
    exit;
  end;
  Console.WriteLine(prompt);
  for i := 0 to High(choices) do
    Console.WriteLine('  ' + IntToStr(i + 1) + ') ' + choices[i]);

  if (defaultIdx >= 0) and (defaultIdx <= High(choices)) then
    defaultHint := IntToStr(defaultIdx + 1)
  else
    defaultHint := '';

  while true do
  begin
    if defaultHint <> '' then
      Console.Write('Choice [' + defaultHint + ']: ')
    else
      Console.Write('Choice: ');
    input := '';
    keyInfo.Key := TConsoleKey.None;
    while true do
    begin
      keyInfo := Console.ReadKey(true);
      if keyInfo.Key = TConsoleKey.Escape then
      begin
        if ConfirmCancel then
        begin
          cancelled := true;
          exit;
        end;
        break;
      end;
      if keyInfo.Key = TConsoleKey.Enter then
      begin
        Console.WriteLine;
        break;
      end;
      if keyInfo.Key = TConsoleKey.Backspace then
      begin
        if Length(input) > 0 then
        begin
          SetLength(input, Length(input) - 1);
          Console.Write(#8 + ' ' + #8);
        end;
        continue;
      end;
      if (keyInfo.KeyChar >= '0') and (keyInfo.KeyChar <= '9') then
      begin
        input := input + keyInfo.KeyChar;
        Console.Write(keyInfo.KeyChar);
      end;
    end;
    if cancelled then
      exit;
    if keyInfo.Key <> TConsoleKey.Enter then
      continue;
    if Trim(input) = '' then
    begin
      if defaultHint <> '' then
      begin
        result := defaultIdx;
        exit;
      end;
      Console.WriteLine('  please enter a number between 1 and ' + IntToStr(Length(choices)));
      continue;
    end;
    selected := StrToIntDef(input, 0);
    if (selected >= 1) and (selected <= Length(choices)) then
    begin
      result := selected - 1;
      exit;
    end;
    Console.WriteLine('  out of range - please enter a number between 1 and ' + IntToStr(Length(choices)));
  end;
end;

end.
