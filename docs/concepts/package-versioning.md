# Package versioning

Packages are versioned using [Semantic Versioning](https://semver.org/).

**Summary**

Given a version number MAJOR.MINOR.PATCH, increment the:

1. MAJOR version when you make incompatible API changes,
2. MINOR version when you add functionality in a backwards compatible manner, and
3. PATCH version when you make backwards compatible bug fixes.

Additional labels for pre-release and build metadata are available as extensions to the MAJOR.MINOR.PATCH format.

**Delphi Challenges**

Now with delphi, it's extremely important to adhere to the rules for maintaining compatibility, especially if the packages are binary (ie thet use dcu's/dcp's etc rather than source files).

Quoting Allen Bauer (ex Emb chief architect)

> Ok, this is an interesting subject and one that many folks seem to think is voodoo and magic. The rules are deceptively simple...
>
> Any material change to an existing interfaced symbol, directly or indirectly, will render that dcu incompatible with prebuilt units that depend on this unit."
>
> Let's break this down a little more. What is a "material" change?

> Anything that could change the layout of a structured type (object, class, record, etc).
> A change in symbols within a structured scope. This means even the addition of a non-virtual method to a base class is a "material" change.
> A change in method or global function signature.
> Remember, I also said indirectly... For instance, a change to a type used as the parameter of a given global function will also cause a compatibility error. Even if that type is the same size and semantic.
>
> The key points here are changes to existing symbols. What this means is that you can, many times, get away with adding another symbol to the unit without rendering it incompatible with existing pre-built dcu files. This is logical be cause there cannot be a reference to a symbol that didn't previously exist. However, this can also backfire, because you once new code references the new symbols, that new code cannot be used with the old version of the dcu. Logically because of the above "material change" rule. Deleting a symbol is a "material" change.
>
> Even if you think you followed all those rules, there can still be cases where dcus can be rendered incompatible. Some of those are the result of a compiler change in a patch that fixes something critical but could render dcus built with the new version incompatible with older versions of the compiler. The newer, fixed version would usually be made resilient enough to load the older dcu files and compile against them, but new dcus written cannot be used with the older compiler. This is rare, but has happened a few times over the last couple of decades.

More guidance will follow on this topic as we gain more experience with this.
