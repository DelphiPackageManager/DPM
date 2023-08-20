{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  {$DEFINE STYLEELEMENTS}
{$IFEND}

{$IF CompilerVersion >= 32.0}
//  {$IF Declared(IOTAIDEThemingServices250)} //doesn't work in 10.2 when it should
    {$DEFINE THEMESERVICES}
//  {$IFEND}
{$IFEND}

{$IF CompilerVersion > 33.0} //10.4 or later
    {$DEFINE USEIMAGECOLLECTION}
    {$DEFINE USEBUTTONIMAGELIST}
{$IFEND}


