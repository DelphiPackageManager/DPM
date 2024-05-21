program DPM.Core.Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  TargetPlatformTests in 'Tests\TargetPlatformTests.pas',
  DPM.Core.Tests.Spec.Reader in 'Tests\DPM.Core.Tests.Spec.Reader.pas',
  TestLogger in 'Tests\TestLogger.pas',
  DependencyVersionTests in 'Tests\DependencyVersionTests.pas',
  DPM.Core.Logging in 'Core\Logging\DPM.Core.Logging.pas',
  DPM.Core.Constants in 'Core\DPM.Core.Constants.pas',
  DPM.Core.Init in 'Core\DPM.Core.Init.pas',
  DPM.Core.TargetPlatform in 'Core\DPM.Core.TargetPlatform.pas',
  DPM.Core.Types in 'Core\DPM.Core.Types.pas',
  DPM.Core.Utils.Config in 'Core\Utils\DPM.Core.Utils.Config.pas',
  DPM.Core.Utils.Directory in 'Core\Utils\DPM.Core.Utils.Directory.pas',
  DPM.Core.Utils.Enum in 'Core\Utils\DPM.Core.Utils.Enum.pas',
  DPM.Core.Utils.Numbers in 'Core\Utils\DPM.Core.Utils.Numbers.pas',
  DPM.Core.Utils.Path in 'Core\Utils\DPM.Core.Utils.Path.pas',
  DPM.Core.Utils.Process in 'Core\Utils\DPM.Core.Utils.Process.pas',
  DPM.Core.Utils.Strings in 'Core\Utils\DPM.Core.Utils.Strings.pas',
  DPM.Core.Utils.System in 'Core\Utils\DPM.Core.Utils.System.pas',
  DPM.Core.Utils.XML in 'Core\Utils\DPM.Core.Utils.XML.pas',
  DPM.Core.Spec.BPLEntry in 'Core\Spec\DPM.Core.Spec.BPLEntry.pas',
  DPM.Core.Spec.BuildEntry in 'Core\Spec\DPM.Core.Spec.BuildEntry.pas',
  DPM.Core.Spec.Dependency in 'Core\Spec\DPM.Core.Spec.Dependency.pas',
  DPM.Core.Spec.DependencyGroup in 'Core\Spec\DPM.Core.Spec.DependencyGroup.pas',
  DPM.Core.Spec.FileEntry in 'Core\Spec\DPM.Core.Spec.FileEntry.pas',
  DPM.Core.Spec.Interfaces in 'Core\Spec\DPM.Core.Spec.Interfaces.pas',
  DPM.Core.Spec.MetaData in 'Core\Spec\DPM.Core.Spec.MetaData.pas',
  DPM.Core.Spec.Node in 'Core\Spec\DPM.Core.Spec.Node.pas',
  DPM.Core.Spec in 'Core\Spec\DPM.Core.Spec.pas',
  DPM.Core.Spec.Reader in 'Core\Spec\DPM.Core.Spec.Reader.pas',
  DPM.Core.Spec.SearchPath in 'Core\Spec\DPM.Core.Spec.SearchPath.pas',
  DPM.Core.Spec.SearchPathGroup in 'Core\Spec\DPM.Core.Spec.SearchPathGroup.pas',
  DPM.Core.Spec.TargetPlatform in 'Core\Spec\DPM.Core.Spec.TargetPlatform.pas',
  DPM.Core.Spec.Template in 'Core\Spec\DPM.Core.Spec.Template.pas',
  DPM.Core.Spec.TemplateBase in 'Core\Spec\DPM.Core.Spec.TemplateBase.pas',
  DPM.Core.Spec.Writer in 'Core\Spec\DPM.Core.Spec.Writer.pas',
  DPM.Core.Options.Base in 'Core\Options\DPM.Core.Options.Base.pas',
  DPM.Core.Options.Cache in 'Core\Options\DPM.Core.Options.Cache.pas',
  DPM.Core.Options.Common in 'Core\Options\DPM.Core.Options.Common.pas',
  DPM.Core.Options.Config in 'Core\Options\DPM.Core.Options.Config.pas',
  DPM.Core.Options.Feed in 'Core\Options\DPM.Core.Options.Feed.pas',
  DPM.Core.Options.Install in 'Core\Options\DPM.Core.Options.Install.pas',
  DPM.Core.Options.List in 'Core\Options\DPM.Core.Options.List.pas',
  DPM.Core.Options.Pack in 'Core\Options\DPM.Core.Options.Pack.pas',
  DPM.Core.Options.Push in 'Core\Options\DPM.Core.Options.Push.pas',
  DPM.Core.Options.Restore in 'Core\Options\DPM.Core.Options.Restore.pas',
  DPM.Core.Options.Search in 'Core\Options\DPM.Core.Options.Search.pas',
  DPM.Core.Options.Sources in 'Core\Options\DPM.Core.Options.Sources.pas',
  DPM.Core.Options.Spec in 'Core\Options\DPM.Core.Options.Spec.pas',
  DPM.Core.Options.UnInstall in 'Core\Options\DPM.Core.Options.UnInstall.pas',
  DPM.Core.Dependency.Context in 'Core\Dependency\DPM.Core.Dependency.Context.pas',
  DPM.Core.Dependency.Reference in 'Core\Dependency\DPM.Core.Dependency.Reference.pas',
  DPM.Core.Dependency.Interfaces in 'Core\Dependency\DPM.Core.Dependency.Interfaces.pas',
  DPM.Core.Dependency.Resolution in 'Core\Dependency\DPM.Core.Dependency.Resolution.pas',
  DPM.Core.Dependency.Resolver in 'Core\Dependency\DPM.Core.Dependency.Resolver.pas',
  DPM.Core.Dependency.Version in 'Core\Dependency\DPM.Core.Dependency.Version.pas',
  DPM.Core.Cache.Interfaces in 'Core\Cache\DPM.Core.Cache.Interfaces.pas',
  DPM.Core.Cache in 'Core\Cache\DPM.Core.Cache.pas',
  DPM.Core.Compiler.EnvironmentProvider in 'Core\Compiler\DPM.Core.Compiler.EnvironmentProvider.pas',
  DPM.Core.Compiler.Factory in 'Core\Compiler\DPM.Core.Compiler.Factory.pas',
  DPM.Core.Compiler.Interfaces in 'Core\Compiler\DPM.Core.Compiler.Interfaces.pas',
  DPM.Core.Compiler.MSBuild in 'Core\Compiler\DPM.Core.Compiler.MSBuild.pas',
  DPM.Core.Configuration.Classes in 'Core\Configuration\DPM.Core.Configuration.Classes.pas',
  DPM.Core.Configuration.Interfaces in 'Core\Configuration\DPM.Core.Configuration.Interfaces.pas',
  DPM.Core.Configuration.Manager in 'Core\Configuration\DPM.Core.Configuration.Manager.pas',
  DPM.Core.Package.Dependency in 'Core\Package\DPM.Core.Package.Dependency.pas',
  DPM.Core.Package.Installer in 'Core\Package\DPM.Core.Package.Installer.pas',
  DPM.Core.Package.InstallerContext in 'Core\Package\DPM.Core.Package.InstallerContext.pas',
  DPM.Core.Package.Interfaces in 'Core\Package\DPM.Core.Package.Interfaces.pas',
  DPM.Core.Package.Classes in 'Core\Package\DPM.Core.Package.Classes.pas',
  DPM.Core.Package.SearchResults in 'Core\Package\DPM.Core.Package.SearchResults.pas',
  DPM.Core.Packaging.Archive in 'Core\Packaging\DPM.Core.Packaging.Archive.pas',
  DPM.Core.Packaging.Archive.Reader in 'Core\Packaging\DPM.Core.Packaging.Archive.Reader.pas',
  DPM.Core.Packaging.Archive.Writer in 'Core\Packaging\DPM.Core.Packaging.Archive.Writer.pas',
  DPM.Core.Packaging.IdValidator in 'Core\Packaging\DPM.Core.Packaging.IdValidator.pas',
  DPM.Core.Packaging in 'Core\Packaging\DPM.Core.Packaging.pas',
  DPM.Core.Packaging.Writer in 'Core\Packaging\DPM.Core.Packaging.Writer.pas',
  DPM.Core.Project.Configuration in 'Core\Project\DPM.Core.Project.Configuration.pas',
  DPM.Core.Project.Editor in 'Core\Project\DPM.Core.Project.Editor.pas',
  DPM.Core.Project.GroupProjReader in 'Core\Project\DPM.Core.Project.GroupProjReader.pas',
  DPM.Core.Project.Interfaces in 'Core\Project\DPM.Core.Project.Interfaces.pas',
  DPM.Core.Repository.Base in 'Core\Repository\DPM.Core.Repository.Base.pas',
  DPM.Core.Repository.Directory in 'Core\Repository\DPM.Core.Repository.Directory.pas',
  DPM.Core.Repository.Factory in 'Core\Repository\DPM.Core.Repository.Factory.pas',
  DPM.Core.Repository.Http in 'Core\Repository\DPM.Core.Repository.Http.pas',
  DPM.Core.Repository.Interfaces in 'Core\Repository\DPM.Core.Repository.Interfaces.pas',
  DPM.Core.Repository.Manager in 'Core\Repository\DPM.Core.Repository.Manager.pas',
  DPM.Core.Sources.Interfaces in 'Core\Sources\DPM.Core.Sources.Interfaces.pas',
  DPM.Core.Sources.Manager in 'Core\Sources\DPM.Core.Sources.Manager.pas',
  DPM.Core.Sources.Types in 'Core\Sources\DPM.Core.Sources.Types.pas',
  DPM.Core.MSXML in 'Core\Xml\DPM.Core.MSXML.pas',
  DPM.Core.XML.NodeBase in 'Core\Xml\DPM.Core.XML.NodeBase.pas',
  DPM.Core.Package.Icon in 'Core\Package\DPM.Core.Package.Icon.pas',
  DPM.Core.Tests.Types in 'Tests\DPM.Core.Tests.Types.pas',
  DPM.Core.Compiler.BOM in 'Core\Compiler\DPM.Core.Compiler.BOM.pas',
  DPM.Core.Compiler.ProjectSettings in 'Core\Compiler\DPM.Core.Compiler.ProjectSettings.pas',
  DPM.Core.Utils.Files in 'Core\Utils\DPM.Core.Utils.Files.pas',
  DPM.Core.Tests.PathUtils in 'Tests\DPM.Core.Tests.PathUtils.pas',
  DPM.Core.Package.Installer.Interfaces in 'Core\Package\DPM.Core.Package.Installer.Interfaces.pas',
  DPM.Core.Sources.ServiceIndex in 'Core\Sources\DPM.Core.Sources.ServiceIndex.pas',
  DPM.Core.Package.ListItem in 'Core\Package\DPM.Core.Package.ListItem.pas',
  DPM.Core.Package.PackageLatestVersionInfo in 'Core\Package\DPM.Core.Package.PackageLatestVersionInfo.pas',
  DPM.Core.Manifest.Interfaces in 'Core\Manifest\DPM.Core.Manifest.Interfaces.pas',
  DPM.Core.Manifest in 'Core\Manifest\DPM.Core.Manifest.pas',
  DPM.Core.Manifest.Reader in 'Core\Manifest\DPM.Core.Manifest.Reader.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := false;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
