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
//want to abort - or if the registered cancellation token is signalled
//(Ctrl-C/Ctrl-Break). Callers must check cancelled and unwind without writing
//anything.

uses
  VSoft.CancellationToken;

type
  TPromptChoices = array of string;

//Register the process cancellation token so blocking key reads wake up and
//report cancelled=True when the user presses Ctrl-C. Pass nil to restore plain
//blocking reads. Set once at startup by the console application.
procedure SetPromptCancellationToken(const token : ICancellationToken);

function PromptLine(const prompt : string; const defaultValue : string; out cancelled : boolean) : string;
function PromptLineRequired(const prompt : string; out cancelled : boolean) : string;
function PromptYesNo(const prompt : string; const defaultYes : boolean; out cancelled : boolean) : boolean;
function PromptChoice(const prompt : string; const choices : TPromptChoices; const defaultIdx : integer; out cancelled : boolean) : integer;

/// <summary>
///  Interactive checkbox list (arrow keys to move, space to toggle, enter to
///  commit, esc to cancel) built over VSoft.AnsiConsole. Each choice starts
///  checked/unchecked per defaultChecked (defaults to unchecked when the array
///  is shorter than choices). On commit, `selected` holds the chosen indices
///  (0-based into choices) and the result is True. On cancel the result is False
///  and cancelled is set. Requires at least one item to be selected to commit.
/// </summary>
function PromptMultiSelect(const prompt : string; const choices : TPromptChoices;
  const defaultChecked : array of boolean; out selected : TArray<integer>; out cancelled : boolean) : boolean;

implementation

uses
  System.SysUtils,
  System.SyncObjs,
  System.Console,
  System.Console.Types,
  VSoft.AnsiConsole,
  VSoft.AnsiConsole.Prompts.MultiSelect;

const
  cCancelPrompt = 'Cancel? (y/N): ';
  cPollIntervalMs = 20;

var
  GCancellationToken : ICancellationToken;

procedure SetPromptCancellationToken(const token : ICancellationToken);
begin
  GCancellationToken := token;
end;

//Blocks until a key is available or the registered cancellation token is
//signalled, then reads and returns that key. When the token fires (Ctrl-C) the
//read is abandoned and cancelled is set True so callers unwind exactly as they
//do for an Esc-confirmed cancel. Without a token registered it falls back to a
//plain blocking read - the original behaviour.
function WaitForKey(out cancelled : boolean) : TConsoleKeyInfo;
begin
  cancelled := false;
  if GCancellationToken = nil then
  begin
    result := Console.ReadKey(true);
    exit;
  end;
  while not Console.KeyAvailable do
  begin
    if GCancellationToken.WaitFor(cPollIntervalMs) = wrSignaled then
    begin
      cancelled := true;
      result := Default(TConsoleKeyInfo);
      exit;
    end;
  end;
  result := Console.ReadKey(true);
end;

function ConfirmCancel : boolean;
var
  keyInfo : TConsoleKeyInfo;
  ch : Char;
  cancelled : boolean;
begin
  Console.WriteLine;
  Console.Write(cCancelPrompt);
  keyInfo := WaitForKey(cancelled);
  if cancelled then
    exit(true);
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
    keyInfo := WaitForKey(cancelled);
    if cancelled then
    begin
      result := '';
      exit;
    end;
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
    keyInfo := WaitForKey(cancelled);
    if cancelled then
      exit;
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
      keyInfo := WaitForKey(cancelled);
      if cancelled then
        exit;
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

function PromptMultiSelect(const prompt : string; const choices : TPromptChoices;
  const defaultChecked : array of boolean; out selected : TArray<integer>; out cancelled : boolean) : boolean;
var
  msPrompt : IMultiSelectionPrompt<integer>;
  i : integer;
  preset : boolean;
  chosen : TArray<integer>;
  sentinel : TArray<integer>;
begin
  cancelled := false;
  SetLength(selected, 0);
  if Length(choices) = 0 then
    exit(false);

  msPrompt := MultiSelectionPrompt<integer>.Create
    .WithTitle(prompt)
    .WithInstructions('up/down move, space toggle, enter confirm, esc cancel')
    .Required(1);

  for i := 0 to High(choices) do
  begin
    if i <= High(defaultChecked) then
      preset := defaultChecked[i]
    else
      preset := false;
    msPrompt.AddChoice(i, choices[i], preset);
  end;

  //Esc returns this sentinel (a single -1) rather than raising EPromptCancelled,
  //so we can detect cancellation without exception handling. Real selections are
  //always non-negative indices, so [-1] is unambiguous.
  SetLength(sentinel, 1);
  sentinel[0] := -1;
  msPrompt.WithCancelResult(sentinel);

  chosen := msPrompt.Show(AnsiConsole.Console);

  if (Length(chosen) = 1) and (chosen[0] = -1) then
  begin
    cancelled := true;
    exit(false);
  end;
  selected := chosen;
  result := true;
end;

end.
