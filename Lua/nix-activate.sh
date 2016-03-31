#!/bin/sh

nix-env -i -A torchPackages.trepl
nix-env -i -A luajitPackages.luarocks
luarocks install 30log --local
eval `luarocks path`

