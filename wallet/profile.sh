cabal new-run ergvein --enable-profiling -- +RTS -hc -L80 -i1.0s
hp2ps -c ergvein.hp
xdg-open ergvein.ps
