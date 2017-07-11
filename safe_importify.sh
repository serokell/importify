#!/usr/bin/env bash

cp "$1" module_backup
./importify file -i "$1"
