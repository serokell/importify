#!/usr/bin/env bash

cp "$1" module_backup
./importify remove -i "$1"
