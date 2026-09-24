unit DPM.Creator.Theme;

interface

const
  //The two styles linked into the exe - see Custom_Styles in DspecCreator.dproj.
  //These are the names stored inside the .vsf files, which is what TStyleManager
  //matches on - not the file names and not the resource names.
  cCreatorDarkStyle = 'Windows11 MineShaft';
  cCreatorLightStyle = 'Windows11 Modern Light';

///<summary>
///  True when Windows is set to dark mode for applications. Dark is the result
///  for every failure path - missing key, missing value, wrong value type, or a
///  registry error - because dark is what DSpecCreator has always shipped with.
///</summary>
function WindowsAppsUseDarkTheme : boolean;

///<summary>
///  The VCL style name to pass to TStyleManager.TrySetStyle at startup.
///</summary>
function GetPreferredStyleName : string;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Win.Registry;

const
  cPersonalizeKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  cAppsUseLightTheme = 'AppsUseLightTheme';

function WindowsAppsUseDarkTheme : boolean;
var
  reg : TRegistry;
begin
  //Default dark - only an actual DWORD reading 1 (light) changes this.
  result := true;
  try
    //No KEY_WOW64_64KEY needed : HKCU\Software is not redirected under WOW64
    //(only HKCU\Software\Classes is), so Win32 and Win64 read the same value.
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_CURRENT_USER;
      //Returns false rather than raising when the key is absent.
      if not reg.OpenKeyReadOnly(cPersonalizeKey) then
        exit;
      try
        //ReadInteger raises on a missing or wrongly typed value, so probe first.
        if not reg.ValueExists(cAppsUseLightTheme) then
          exit;
        if not (reg.GetDataType(cAppsUseLightTheme) in [rdInteger, rdIntegerBE]) then
          exit;
        //0 = dark apps, 1 = light apps. Anything but 1 is treated as dark.
        result := reg.ReadInteger(cAppsUseLightTheme) <> 1;
      finally
        reg.CloseKey;
      end;
    finally
      reg.Free;
    end;
  except
    //Policy lockdown or an unloaded hive is not worth an exception before the
    //main form even exists.
    on E : Exception do
      result := true;
  end;
end;

function GetPreferredStyleName : string;
begin
  if WindowsAppsUseDarkTheme then
    result := cCreatorDarkStyle
  else
    result := cCreatorLightStyle;
end;

end.
