# Amazonka

## Scrive Update

The original code version is the same as of released amazonka 1.6.1.

https://github.com/brendanhay/amazonka/commit/d6120081222808ca6173b28b07d13b1721a7fc07

That commit corresponds to the latest master branch of that repo. Then they started
using "main" as the main development branch.

This repo contains a lightweight version of amazonka based on 1.6.1 with necessary
fixes for GHC 9.2.x.

## Contents

* [Description](#description)
* [Documentation](#documentation)
* [Organisation](#organisation)
* [Change Log](#change-log)
* [Contribute](#contribute)
    - [Package Names](#package-names)
* [Licence](#licence)

## Description

A comprehensive Amazon Web Services SDK for Haskell supporting all of the
publicly available services.

Parts of the code contained in this repository are auto-generated and
automatically kept up to date with Amazon's latest service APIs.

An introductory blog post detailing some of the motivation and design decisions
can be found [here](http://brendanhay.nz/amazonka-comprehensive-haskell-aws-client).

## Documentation

You can find the latest stable release documentation for each respective library
on Hackage under the [AWS section](http://hackage.haskell.org/packages/#cat:AWS).

Haddock documentation which is built by CI from the `develop` branch
can be found [here](http://brendanhay.nz/amazonka-doc).


## Organisation

This repository is organised into the following directory structure:

* [`amazonka`](amazonka): Actual operational logic, you'll need to import this to send requests etc.
* `amazonka-*`: Data types for each of the individual Amazon Web Service libraries.
* `amazonka-*/test`: Tests and fixtures for each respective library.
* [`core`](core): The `amazonka-core` library upon which each of the services depends.
* [`examples`](examples): A currently sparse collection of examples for the various services.
* [`gen`](gen): The code generation binary, along with configuration, templates, and assets.
* [`script`](script): CI scripts to manage the release lifecycle of the service libraries.
* [`share`](share): Makefile plumbing common to all service libraries
* [`test`](test): The `amazonka-test` library containing common test functionality.


## Change Log

A change log for the entire project can be found under [`amazonka/CHANGELOG.md`](amazonka/CHANGELOG.md).


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

### Package Names

It is often desirable to provide supplemental functionality to `amazonka` as
an additional library, for example providing S3 encryption via a package such
as `amazonka-s3-encryption`.

I ask that authors of these packages carefully consider package naming
and preferably do not prefix the package with `amazonka-*` to avoid potential
collisions with generated package names.


## Licence

Amazonka is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
