# importify — manage Haskell imports easily

[![Build Status](https://travis-ci.org/serokell/importify.svg)](https://travis-ci.org/serokell/importify)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Coverage Status](https://coveralls.io/repos/github/serokell/importify/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/mantis?branch=master)

## What this project about?

`importify` tool helps you to manage import section of your Haskell project modules.
GHC compiler can warn you about unused imports. It's a good thing to remove such
imports. But this becomes tedious especially if you use explicit import lists.

**Importify** can remove unused imports automatically.

> TODO: describe what else we want it to do?

## Installation

Installation process assumes that you have already installed and configured `stack`
build tool. Currently `importify` works only with projects built with `stack`.

Perform next steps before driving:

```bash
$ git clone https://github.com/serokell/importify.git  # 1. Clone repository locally
$ cd importify                                         # 2. Step into folder
$ stack install importify\:exe\:importify              # 3. Copy executable under ~/.local/bin
```

## Usage

In short:

```bash
$ cd my-project-which-build-with-stack
$ importify cache
$ importify file path/to/File/With/Unused/Imports.hs
```

`importify` has several commands. Most important is

```
importify --help
```

Before removing redundant imports run `importify cache` command. Importify stores
local cache for project under `.importify` folder inside your project. This cache
stores exported entities for each module for every dependency and for all your
local packages. Make sure to rerun `importify cache` if you change list of
exported functions and types in your project modules. Cache is built incrementally,
it builds dependencies only once. But if you add dependencies or use another versions
of them (for example by bumping stack lts) you need to run `importify cache` again.
Though you can always perform `rm -rf .importify` before caching if you face any
troubles.

After cache is built you can use `importify file PATH_TO_FILE` command from your
project root directory. This command prints output of processed file into
terminal. If you want to change file in-place use next command:

```
importify file -i PATH_TO_FILE
```