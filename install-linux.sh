#!/bin/bash

if [ ! -d "/usr/share/doc/Idlewild-Lang/Examples/" ]; then
  mkdir "/usr/share/doc/Idlewild-Lang/"
  mkdir "/usr/share/doc/Idlewild-Lang/Examples/"
fi

cp idlewild-lang /usr/bin/
cp libkoshka.core.so /usr/lib/
cp libkoshka.mm.so /usr/lib/
cp nasm /usr/bin/
cp fasm /usr/bin/

cp Examples/*.bb /usr/share/doc/Idlewild-Lang/Examples/
