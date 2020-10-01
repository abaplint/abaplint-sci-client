# abaplint SCI client

## Purpose

This is a toolset to run [abaplint](https://abaplint.org) checks within an SAP Code Inspector(SCI) or ABAP Test Cockpit(ATC) run.

![sci](docs/img/sci-sample.png)

It consist of two parts:
1. the abap client (this repo), which integrates with SCI and posts the code to the server part
2. which runs the abaplint (with [NodeJs](https://nodejs.org)), [abaplint-sci-server](https://github.com/abaplint/abaplint-sci-server)

![landscape](docs/img/landscape.png)

**Important:** the code under test leaves your abap instance! Be sure to use secure and controllable abaplint server. For a test you might use common one: [http://sci.abaplint.org/](http://sci.abaplint.org/) (but please don't post any proprietary code).

Dependencies from `abaplint.json` are ignored when running via SCI, instead the dependencies are taken from the running ABAP system.

## Documentation

* [Installation](docs/installation.md)
* [Exporting Dependencies](docs/export_dependencies.md)

## Posts

- https://blogs.sap.com/2020/02/01/running-abaplint-from-sci-atc-adt/
