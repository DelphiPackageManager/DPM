# DPM error and warning codes

Every error and warning DPM reports carries a code, shown in front of the message:

```
error DPME0001: <message>
warning DPMW0004: <message>
```

- `DPME` codes are errors. An error means the operation failed.
- `DPMW` codes are warnings. The operation carried on, but something needs your attention.

This is the same layout MSBuild uses, so CI systems that recognise MSBuild errors pick DPM's up too.

When an operation fails, DPM reports the error that caused it. Search this page for the code to find
out what it means and what to do about it. Messages without a code (`error: ...`) come from parts of DPM
that haven't been given codes yet.

## How the numbers are organised

Errors and warnings share one set of numbers, so a number is used by only one code: an error and a
warning never have the same number. The first digit says which part of DPM reported the problem.

| Range | Area |
|---|---|
| 0xxx | General: the command line, options and configuration |
| 1xxx | Package sources and repositories |
| 2xxx | Dependency resolution |
| 3xxx | Install, restore, uninstall and the package cache |
| 4xxx | Project files (.dproj, .groupproj) |
| 5xxx | Compiling packages |
| 6xxx | Package specs (.dspec), pack and push |
| 7xxx | Signing, trust and verification |
| 8xxx | SBOM and vulnerability scanning |
| 9xxx | The IDE plugin, DSpec Creator and the MCP server |

Once a code is released it is never renumbered or reused.

## General (0xxx)

### DPME0001

**An unexpected exception occurred.**

DPM hit an error that nothing in it was expecting. The message holds the exception's text, and its class
name when that's more specific than `Exception`.

**What to do:** If the message doesn't point to something you can fix, such as a file that is locked or a
disk that is full, please report it as a DPM bug and include the full output.

### DPME0002

**The operation failed without reporting why.**

Something failed but didn't report an error explaining why. DPM adds this code so that a failure is never
silent.

**What to do:** This is always a bug in DPM. Please report it with the full output, run with
`--verbosity=debug` if you can.

### DPME0003

**Internal error.**

DPM found itself in a state that should not be possible.

**What to do:** Please report it as a DPM bug and include the full output.

### DPMW0004

**The operation was cancelled.**

You cancelled the operation (Ctrl+C on the command line, or Cancel in the IDE) before it finished. Any
changes it hadn't finished making were not saved.
