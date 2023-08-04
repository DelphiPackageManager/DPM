
//NOTE : This must be included after the used clause, and the uses clause needs
//to include ToolsApi

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  {$DEFINE STYLEELEMENTS}
{$IFEND}

//{$IF NOT Declared(IBorlandIDEServices) }
//  ToolsAPI must be in uses clause.
//{$IFEND}


{$IF CompilerVersion >= 32.0}
//  {$IF Declared(IOTAIDEThemingServices250)} //doesn't work in 10.2 when it should
    {$DEFINE THEMESERVICES}
//  {$IFEND}
{$IFEND}

{$IF CompilerVersion > 33.0} //10.4 or later
    {$DEFINE USEIMAGECOLLECTION}
{$IFEND}

