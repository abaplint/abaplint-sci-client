# Export Dependenies
Serialize dependencies of a package structure to git.

Use report `ZABAPLINT_DEPENDENCIES`

**Warning:** Do not specify the git repository of the package as the target! Everything in the target repository will be overwritten!

To ensure that all where-used references for SAP objects are correct, run the ABAP report `SAPRSEUB` in background once.
It is more efficient to have a seperate dependency repository for each [abapGit](https://abapgit.org) repository.

For use in connection with static code analysis, method implementations of dependencies are excluded in the files.

## Example
* ABAP development is done in package `$PROJECT`, and stored in repository https://github.com/user/project/
* Create a new repository project_deps to store the dependencies
* Run report with input `$PROJECT` and repository https://github.com/user/project_deps/, note that files in project_deps will be overwritten without any warnings