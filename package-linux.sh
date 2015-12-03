#!/bin/bash

rm -r "$1"
mkdir "$1"
cp *.hs *.c *.cpp *.h "$1"
cp Makefile "$1"
cp install-linux.sh "$1"
cp install-windows.bat "$1"
cp -r Examples/ "$1"
cp README.md "$1"
cp LICENSE "$1"
tar -czf "$1".tar.gz "$1"
