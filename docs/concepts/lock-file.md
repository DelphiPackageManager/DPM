# Lock File

Note : this feature is not yet fully implemented.

## What is a lock file

The lock file records the package dependency resolution for all direct and transitive dependencies of your project. If a lock file is not present when dpm install or restore is run, it will be automatically generated during the dependency resolution process.

## Why do I need this

You should commit the lock file to your version control system, so you can ensure that other developers on your team, and build/ci servers will end up with a reliable restore process, regardless of where or when the restore is done.

Wihout this, only the directly referenced packages are guaranteed to be installed with the same version as you intended. Transitive dependencies (ie dependencies of the referenced packages, and their dependencies) will be resolved again through the package resoltution process, which may result in different versions of packages being used on differnt machines (if a compatible library update was released). Essentially, you may be using different code to other team members and your CI server may be using different versions. This would be considered unreliable.

The lock file is what ensures the consisitent and reliable restore process.

## When can I use it

When it's ready. The dependency resolution algorithm is rather complex (and somewhat messy right now), and needs to be reworked to enable this feature.
