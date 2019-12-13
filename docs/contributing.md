# Contributing

We welcome contributions, however before submitting any changes, create an issue on github so we can discuss your ideas, bugs etc. That way we all avoid wasting effort.

We make extensive use of Spring4D for Dependency Injection and collections. Make sure you are up to speed with DI and use it where possible.

- Use extremely defensive coding style, with useful/detailed error messages.
- Pass an ILogger in everywhere. There is one available through Dependency Injection.
- Start with high level classes for functionality, that take an options object for the command.
- Do not use the RTL generic collections, we're using spring4d ones (interfaced).
- Use interfaces where possible and only expose what is needed.
- Use unit namespaces, but avoid prefixs or namespacing of types where possible (makes for simpler renaming).
- Since we're targeting multiple compiler versions, avoid new language features or rtl features.
- Do not add/use new external libraries before checking with the team. Whilst using libraries is unavaiodable, we would like to keep the dependencies as low as possible. Many of the libraries listed below were created for this project.
