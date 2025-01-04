#!/usr/bin/env sh
LC_ALL="C.UTF-8" LANG="C.UTF-8" ghcid -c "cabal repl gpio-sim gpio-sim-spec" --poll=1 -W -T ":! cabal test gpio-sim-spec" -T "withDeviceInfo defaultGPIOSimOptions exampleDeviceSpec print"
