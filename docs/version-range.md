# Version Range

A Version Range is used when defining package dependencies, it defines the range of versions that are compatible.

This is modelled off the nuget VersionRange, however we only allow the patch version to float, as there a lot of issue with maintaining compatibility in Delphi.

Rules

A version Range uses square [ ] brackets to indicate inclusive and round () brackets to indicate exclusive.

A version range must have at least one and at most 2 valid semantic versions, separated by a comma. Whitespace is ignored. A version range starts and ends with brackets as defined above, except when a single exact semantic version is used, then it must not use brackets (ie like the first example below).

| Range           | Description                                                   |
| --------------- | ------------------------------------------------------------- |
| 1.0.12          | exact version match                                           |
| [1.0.12,]       | inclusive - any version >= 1.0.12 and < 1.1 is compatible     |
| [1.0.12,1.0.57] | inclusive - any version >= 1.0.12 and <= 1.0.57 is compatible |
| (1.0.12,)       | exclusive - any version > 1.0.12 and < 1.1.0 is compatible    |
| (1.0.12,1.0.57) | exclusive - any version > 1.0.12 and < 1.0.57 is compatible   |
| [1.0.12,1.0.57) | mixed - any version >= 1.0.12 and < 1.0.57 is compatible      |
| (1.0.12,1.0.57] | mixed - any version > 1.0.12 and <= 1.0.57 is compatible      |
| (1.0.12,1.0.13) | invalid - exlusive but empty range                            |
| (1.0.12)        | invalid - exlusive but empty range                            |
| (,1.0.12)       | no min version specified.                                     |
