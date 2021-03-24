# Qwdefrontend
There's two applications here, one `client` that takes Haskell code to javascript, and one `server` that can work backend to get data (from the `../backend`). The `client` needs to be compiled with ghcjs, and the `server` needs to be compiled with ghc. We're using [miso](https://github.com/dmjio/miso), which has an [elm architechture](https://guide.elm-lang.org/architecture/). The server and client are [isomorphic](https://en.wikipedia.org/wiki/Isomorphic_JavaScript).

To build I recommend [getting nix](https://nixos.org/download.html), or you can use the docker image (`docker build`)

Thanks to [miso](https://github.com/dmjio/miso) for inspiration
