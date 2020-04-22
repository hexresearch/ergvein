cabal new-run ergvein --enable-profiling -- +RTS -h
hp2ps ergvein.hp 
xdg-open ergvein.ps
