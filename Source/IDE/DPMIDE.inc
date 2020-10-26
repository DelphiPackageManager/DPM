
//NOTE : This must be included after the used clause, and the uses clause needs
//to include ToolsApi

{$IF CompilerVersion >=32.0}
  {$IF Declared(IOTAIDEThemingServices)}
    {$DEFINE THEMESERVICES}
  {$IFEND}
{$IFEND}

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  {$DEFINE STYLEELEMENTS}
{$IFEND}
