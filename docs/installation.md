# Installation

* Prerequisites:
    * abapGit dev version must be installed (with all the separate classes, not a simple file version)
    * ABAP731 required, potentially works with 702. Though downported json xml parser should be available.
    * Install [ajson](https://github.com/sbcgua/ajson) package using abapGit (dependency)
* Install on ABAP backend (this repo) using [abapGit](https://abapgit.org)
* Activate check in transaction `SCI` -> Management of -> Tests
* Adjust check variants, maintain connection to [abaplint-sci-server](https://github.com/abaplint/abaplint-sci-server), report `ZABAPLINT_CONFIG`
* abaplint settings are defined per target package

![setup1](img/setup1.png)
![setup2](img/setup2.png)