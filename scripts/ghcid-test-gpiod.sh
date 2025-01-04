#!/usr/bin/env sh
LC_ALL="C.UTF-8" LANG="C.UTF-8" ghcid -c "cabal repl gpiod gpiod-spec" --poll=1 -W -T ":! cabal test gpiod-spec"
