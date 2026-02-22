#!/usr/bin/env bash
set -euo pipefail

PROJECT_PATH="$(cd "$(dirname "$0")" && pwd)"
GC_ROOT_PATH="$PROJECT_PATH/nix-shell-root"
NIX_FILE="$PROJECT_PATH/default.nix"
NEED_REGEN=false

if [ ! -f "$NIX_FILE" ] || [ ! -s "$NIX_FILE" ]; then
  NEED_REGEN=true
elif [ "$PROJECT_PATH/default.R" -nt "$NIX_FILE" ]; then
  NEED_REGEN=true
elif [ "$PROJECT_PATH/DESCRIPTION" -nt "$NIX_FILE" ]; then
  NEED_REGEN=true
fi

if [ "$NEED_REGEN" = true ]; then
  echo "Regenerating default.nix from default.R"
  nix-shell \
    --expr "let pkgs = import <nixpkgs> {}; in pkgs.mkShell {\
      buildInputs = [ pkgs.R pkgs.rPackages.rix pkgs.rPackages.cli\
                      pkgs.rPackages.curl pkgs.curlMinimal pkgs.cacert ]; }" \
    --command "cd '$PROJECT_PATH' && Rscript --vanilla default.R"
fi

echo "Building shell and creating GC root"
nix-build "$NIX_FILE" -A shell -o "$GC_ROOT_PATH" --quiet

echo "Entering nix shell"
exec nix-shell "$NIX_FILE"
