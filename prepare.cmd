@echo off

net session >nul 2>&1
if %ERRORLEVEL% GTR 0 (
    echo Administration rights required for using mklink.exe command!
    echo You are NOT Administrator. Exiting...
    timeout 10
    exit /B 1
)

%~d0
cd %~dp0

if not exist Source goto wrong_dir

echo Scan for git.exe in path...
where /q git.exe
if %ERRORLEVEL% GTR 0 (
    echo git.exe command not found in path!
    echo Please install from https://gitforwindows.org/
    timeout 10
    exit /B 1
)

echo Check JsonDataObjects...
if not exist ..\JsonDataObjects git clone https://github.com/ahausladen/JsonDataObjects.git ..\JsonDataObjects

echo Check OmniThreadLibrary...
if not exist ..\OmniThreadLibrary git clone https://github.com/gabr42/OmniThreadLibrary.git ..\OmniThreadLibrary

echo Check Spring4D...
if not exist ..\spring4d git clone --branch develop https://bitbucket.org/sglienke/spring4d.git ..\spring4d

echo VSoft.AntPatterns...
if not exist ..\VSoft.AntPatterns git clone https://github.com/VSoftTechnologies/VSoft.AntPatterns.git ..\VSoft.AntPatterns

echo VSoft.Awaitable...
if not exist ..\VSoft.Awaitable git clone https://github.com/VSoftTechnologies/VSoft.Awaitable ..\VSoft.Awaitable

echo Check VSoft.CancellationToken...
if not exist ..\VSoft.CancellationToken git clone https://github.com/VSoftTechnologies/VSoft.CancellationToken ..\VSoft.CancellationToken

echo Check VSoft.CommandLineParser...
if not exist ..\VSoft.CommandLineParser git clone https://github.com/VSoftTechnologies/VSoft.CommandLineParser.git ..\VSoft.CommandLineParser

echo Check VSoft.HttpClient...
if not exist ..\VSoft.HttpClient git clone https://github.com/VSoftTechnologies/VSoft.HttpClient.git ..\VSoft.HttpClient

echo Check VSoft.Messaging...
if not exist ..\VSoft.Messaging git clone https://github.com/VSoftTechnologies/VSoft.Messaging.git ..\VSoft.Messaging

echo Check VSoft.SemanticVersion...
if not exist ..\VSoft.SemanticVersion git clone https://github.com/VSoftTechnologies/VSoft.SemanticVersion.git ..\VSoft.SemanticVersion

echo Check VSoft.Uri...
if not exist ..\VSoft.Uri git clone https://github.com/VSoftTechnologies/VSoft.Uri.git ..\VSoft.Uri

if not exist Source\Libs mkdir Source\Libs
cd Source\Libs

if exist ..\..\..\JsonDataObjects mklink /d JsonDataObjects ..\..\..\JsonDataObjects
if exist ..\..\..\OmniThreadLibrary mklink /d OmniThreadLibrary ..\..\..\OmniThreadLibrary
if exist ..\..\..\Spring4D mklink /d Spring4D ..\..\..\Spring4D
if exist ..\..\..\VSoft.AntPatterns mklink /d VSoft.AntPatterns ..\..\..\VSoft.AntPatterns
if exist ..\..\..\VSoft.Awaitable mklink /d VSoft.Awaitable ..\..\..\VSoft.Awaitable
if exist ..\..\..\VSoft.CancellationToken mklink /d VSoft.CancellationToken ..\..\..\VSoft.CancellationToken
if exist ..\..\..\VSoft.CommandLineParser mklink /d VSoft.CommandLineParser ..\..\..\VSoft.CommandLineParser
if exist ..\..\..\VSoft.HttpClient mklink /d VSoft.HttpClient ..\..\..\VSoft.HttpClient
if exist ..\..\..\VSoft.Messaging mklink /d VSoft.Messaging ..\..\..\VSoft.Messaging
if exist ..\..\..\VSoft.SemanticVersion mklink /d VSoft.SemanticVersion ..\..\..\VSoft.SemanticVersion
if exist ..\..\..\VSoft.Uri mklink /d VSoft.Uri ..\..\..\VSoft.Uri

echo done...
goto byebye

:wrong_dir
echo Error: Invalid Location. Source directory not found.
goto byebye

:byebye
timeout 10
