# importify — manage Haskell imports easily

[![Build Status](https://travis-ci.org/serokell/importify.svg)](https://travis-ci.org/serokell/importify)

## What this project about?

`importify` tool helps you to manage import section of your Haskell project modules.
GHC compiler can warn you about unused imports. It's a good thing to remove such
imports. But this becomes tedious especially if you use explicit import lists.

**Importify** can remove unused imports automatically. Below you can see example.

Before importify |  After importify
:---------------:|:-----------------:
![Module with imports mess](https://user-images.githubusercontent.com/4276606/29321624-b6c2e11a-81e3-11e7-9003-da2a399c9161.png) | ![After removing all unused imports](https://user-images.githubusercontent.com/4276606/29321628-b98afb30-81e3-11e7-855f-3430fe9d250f.png)

You can use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) after `importify` to prettify imports.

> TODO: describe what else we want importify to do?

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