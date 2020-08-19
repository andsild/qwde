# Qwde frontend
There's two applications here, one `client` that takes Haskell code to javascript, and one `server` that writes the DOM logic. The `client` needs to be compiled with ghcjs, and the `server` needs to be compiled with ghc.

To build I recommend getting nix, or you can use the docker image

The idea:
### Build commands
I usually fire up a `screen` with three terminals.
```bash
# Terminal 1: build server on all changes
nix-shell release.nix --command "ag -l --no-color | grep -E \"shared|server\" | entr sh -c 'cabal build'"
# Terminal 2: build client on all changes
nix-shell release.nix --command "ag -l --no-color | grep -E \"shared|client\" | entr sh -c 'cabal build --ghcjs'"
# Terminal 3: compile and launch project
nix-build release.nix && ./result/bin/server
```
