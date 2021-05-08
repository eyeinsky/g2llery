#!/usr/bin/env bash

main () {
    mkdir -p .dev

    local NIX_EXP_PATH
    for PKG in render identifiers multiline web-browser web-url web; do
        NIX_EXP_PATH=".dev/${PKG}.nix"
        if [[ -f "$NIX_EXP_PATH" ]]; then
            echo Skipping $NIX_EXP_PATH
        else
           cabal2nix https://github.com/eyeinsky/fw.git --subpath "$PKG" \
                     > "$NIX_EXP_PATH"
        fi
    done

    NIX_EXP_PATH='.dev/rapid.nix'
    [[ ! -f "$NIX_EXP_PATH" ]] \
        && cabal2nix https://github.com/eyeinsky/rapid.git > "$NIX_EXP_PATH" \
        || echo Skipping $NIX_EXP_PATH

    hpack
    cabal2nix --shell . > shell.nix
    patch shell.nix dev/shell.nix.patch

    nix-shell --command 'cabal repl'
}


REPO_PATH="$(cd $(dirname $0)/.. && pwd)"
cd "$REPO_PATH"
main
