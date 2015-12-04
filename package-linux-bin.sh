#!/bin/bash

rm -r "$1"
mkdir "$1"
cp idlewild-lang "$1"
cp libkoshka.core.so "$1"
cp libkoshka.mm.so "$1"
cp install-linux.sh "$1"
cp -r Examples/ "$1"
cp README.md "$1"
cp LICENSE "$1"
#tar -czf "$1".tar.gz "$1"
zip -r "$1".zip "$1"
