#!/usr/bin/env bash
NIXPKGS_ALLOW_UNFREE=1 nix-shell -p steam-run --run "steam-run ./bonus/lambdananas -- .."
