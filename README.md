# DPM - A package manager for Delphi

## Introducing DPM

DPM is an open-source package/library manager for Delphi XE2 or later. It is heavily influenced by NuGet, so the cli, docs etc will seem very familiar to NuGet users. Delphi's development environment is quite different from .net, and has different challenges to overcome, so whilst we were influenced by NuGet, DPM is not identical to NuGet. We also took a close look at many other package managers for other development eco-systems.

## What is a Package Manager

A package manager provides a standard for developers to share and consume code. Authors create packages that other developers can consume. The package manager provides a simple way to automate the installation, upgrading or removal of packages. This streamlines the development process, allowing developers to get up and running on a project quickly, without needing to understand the (usually ad-hoc) way the project or organization has structured their third party libraries. This also translates into simpler build/CI processes, with less 'compiles on my machine' style issues.

## Who and Why

DPM's initial developer is Vincent Parrett (DUnitX, FinalBuilder, Continua CI). Why is discussed in [this blog post](https://www.finalbuilder.com/resources/blogs/delphi-package-manager-rfc).

## DPM Status

DPM is in BETA, so whilst most features should work, there may be bugs. We're using it in production to build FinalBuilder and Automise.

### What works

- Creating packages (library authors)
- Pushing packages to a package source (directory or server)
- Installing and Restoring packages, including dependencies and design time components.
- Multiple package sources
- Package Signing - both Author (using a code signing certificate) and Repository signing (automatic).
- Repository and Author Trust.
- SBOM generation.
- CLI and IDE plugin clients.

## How do I use it

See [getting started](https://docs.delphi.dev/getting-started/installing.html).

The command line documentation can be found [here](https://docs.delphi.dev/commands/commands.html).

## Is DPM integrated into the Delphi IDE

Yes, the installer available under the releases section includes IDE plugins for XE2-13.0

## Is there a central package source

Yes, you can [find it here](https://delphi.dev) - you only need to signup if you intend to publish packages.

## Is my old version of Delphi supported

Delphi XE2-13.1 are currently supported.

All target [platforms](https://docs.delphi.dev/platforms.html) for supported compiler versions are supported.

## What about C++ Builder or FPC

Whilst we would like to support C++Builder, we would need some help - we're delphi people.
[see here](https://docs.delphi.dev/compiler-versions.html)

## Does it support design-time components

YES.

## How does it work

See [this page](https://docs.delphi.dev/concepts/how-it-works.html)

## Can I help

Yep, see [Contributing to DPM](https://docs.delphi.dev/contributing.html).
