#!/bin/bash

IWL_HOME=$(echo $1 | tr -d '"')
IWL_LIBS=$(echo $2 | tr -d '"')
IWL_DOCS=$(echo $3 | tr -d '"')

if [ ! -d $IWL_HOME ]; then
  mkdir $IWL_HOME
fi

if [ ! -d $IWL_DOCS ]; then
  mkdir $IWL_DOCS
  mkdir "$IWL_DOCS/Examples/"
fi

cp idlewild-lang $IWL_HOME
cp libkoshka.core.so $IWL_LIBS
cp libkoshka.mm.so $IWL_LIBS
cp nasm $IWL_HOME
cp fasm $IWL_HOME
cp iwlls $IWL_HOME
cp iwldi.asm $IWL_HOME
cp Examples/*.bb $IWL_DOCS/Examples
