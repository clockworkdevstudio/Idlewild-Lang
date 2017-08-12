#!/bin/bash
if [ ! -d "/usr/local/bin/" ]; then
  mkdir "/usr/local/bin/"
fi
if [ ! -d "/usr/local/lib/" ]; then
  mkdir "/usr/local/lib/"
fi

if [ ! -d "/usr/local/doc/Idlewild-Lang/Examples" ]; then
  mkdir "/usr/local/doc/"
  mkdir "/usr/local/doc/Idlewild-Lang/"
  mkdir "/usr/local/doc/Idlewild-Lang/Examples/"
fi

cp idlewild-lang-mac-os /usr/local/bin/
if [ ! -f /usr/local/bin/idlewild-lang ]; then
  ln /usr/local/bin/idlewild-lang-mac-os /usr/local/bin/idlewild-lang
fi
cp libkoshka.core.dylib /usr/local/lib/
cp libkoshka.mm.dylib /usr/local/lib/
cp -r SDL2.framework /Library/Frameworks/
cp -r SDL2_image.framework /Library/Frameworks/
cp -r SDL2_mixer.framework /Library/Frameworks/
cp -r SDL2_ttf.framework /Library/Frameworks/
cp nasm /usr/local/bin/
cp Examples/*.bb /usr/local/doc/Idlewild-Lang/Examples/

if ! grep -Fxq "export PATH=\$PATH:/usr/local/bin/ # this line was generated automatically by the Idlewild-Lang install script" ~/.bash_profile; then
  echo -e "\nexport PATH=\$PATH:/usr/local/bin/ # this line was generated automatically by the Idlewild-Lang install script" >> ~/.bash_profile
fi

if ! grep -Fxq "export PATH=\$PATH:/usr/local/lib/ # this line was generated automatically by the Idlewild-Lang install script" ~/.bash_profile; then
  echo -e "\nexport PATH=\$PATH:/usr/local/lib/ # this line was generated automatically by the Idlewild-Lang install script" >> ~/.bash_profile
fi
