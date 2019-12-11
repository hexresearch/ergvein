#!/bin/bash

for x in $(find dist-newstyle -name setup-config | grep '/x/.*/noopt/setup-config$' | sed 's|/x/.*/noopt/setup-config$||g');
do
    cd $x;
    for y in $(find x -name setup-config);
    do
      ln -fs $y setup-config;
    done;
    cd -;
done

for x in $(find dist-newstyle -name setup-config | grep '/noopt/setup-config$' | sed 's|/noopt/setup-config$||g');
do
  ( cd $x;     ln -fs noopt/setup-config setup-config;   );
done