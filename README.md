# Legicash FaCTS

Here is the whitepaper that describes the motivation and architecture for
Legicash FaCTS:

[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS)
by [Legicash](http://legi.cash/)

If you'd like to build the Legicash software, follow these instructions.

### Toolchain

First, we need some basic libraries that can be installed as follows on Debian-based distributions:
```
sudo apt-get install -y unzip aspcud libgmp-dev dh-autoreconf opam
```
Next, build the secp256k1 library from sources:
```
  ./scripts/install-secp256k1.sh
```
Note that on Debian and perhaps other distros, as of April 2018, the available secp256k1-dev package is out-of-date and won't work.

Install OCaml via opam:
```
opam init
opam switch 4.06.1
eval `opam config env`
opam install -y cryptokit jane-street-tests lens ppxfind ppx_deriving secp256k1
```
Last, build and run Legicash:
```
make
./src/_build/default/hello_legicash.exe
```

If you'd like to contribute to the Legicash codebase, please submit a
Gitlab merge request, and note the following:

### Coding Style

We'll be using ocamlformat to keep code well-formatted, but otherwise following the
[Jane Street coding standards](https://opensource.janestreet.com/standards/).
