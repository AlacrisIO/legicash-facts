# Legicash FaCTS

[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS)
by [Legicash](http://legi.cash/)

### Toolchain

Using [OPAM](https://opam.ocaml.org/), install the following:
  * OCaml 4.06.1 (Tezos requires this version)
  * jbuilder (dune)
  * ocamlformat
  * Merlin
  * Tuareg

### Coding Style

We'll be using ocamlformat, but otherwise following the
[Jane Street coding standards](https://opensource.janestreet.com/standards/).

### Tezos dependencies

If you want to build Tezos, use OPAM to install:
  * calendar
  * cohttp-lwt-unix
  * depext
  * ezjsonm
  * ipaddr
  * irmin
  * jbuilder
  * lwt
  * mtime
  * nocrypto
  * ocp-ocamlres
  * ocplib-endian
  * ocplib-json-typed
  * omake
  * ounit
  * re
  * ssl
  * stringext

Also, clone *leveldb* from https://github.com/mfp/ocaml-leveldb, and build
using *omake*. There is an OPAM package for *leveldb*, but it is out-of-date, and
does not build with OCaml 4.06.1. For your Linux distro, you may need to install
*libsnappy-dev* for this build to succeed.

With all of the above installed, you can run *make*.

To install Tezos, you also need, via OPAM:
  * opam-installer

With that additional package installed, you can run *make install*.
