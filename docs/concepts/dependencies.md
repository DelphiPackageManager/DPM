## Dependencies

DPM packages define the other packages that they depend on, so that DPM knows what else to install.

A dependency is defined in the package [dspec file](../dspec.md) and consists of

- A package id (e.g) VSoft.CommandLine
- A version range (a range of versions that are compatible)

The dpm dependency resolution process reads this information when installing or restoring packages, using a breadth-first search. It will attempt to resolve and install the highest compatible versions that satisfy all dependencies. The algorithm will detect package version conflicts and report them during the install/restore process.
