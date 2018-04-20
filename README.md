1;4205;0c# Legicash FaCTS

[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS)
by [Legicash](http://legi.cash/)

### Toolchain

First, we need some basic libraries that can be installed as follows on Debian-based distributions:
```
sudo apt-get install -y libgmp-dev libleveldb-dev libsnappy-dev libsecp256k1-dev
```

Then, using [OPAM](https://opam.ocaml.org/), we need to install the following:
  * OCaml 4.06.1 (Tezos requires this version)
  * jbuilder (a.k.a. dune)
  * ocamlformat
  * Merlin
  * Tuareg

```
opam update
opam switch 4.06.1
eval `opam config env`
opam pin -y add ocplib-json-typed --dev # temporary
opam install -y jbuilder ocamlformat merlin tuareg \
    calendar cohttp-lwt-unix cryptokit depext ezjsonm ipaddr \
    irmin jbuilder leveldb lwt mtime nocrypto \
    ocp-ocamlres ocplib-endian ocplib-json-typed \
    omake opam-installer ounit re ssl stringext
```

### Coding Style

We'll be using ocamlformat, but otherwise following the
[Jane Street coding standards](https://opensource.janestreet.com/standards/).

### Tezos dependencies

Note: Tezos provides its own install and build instructions at

  http://doc.tzalpha.net/introduction/howto.html
  
which describes how to use a Tezos-specific OPAM switch and 
automated installation of dependencies. That did not succeed for 
@psteckler1, and maybe it's better not to tie the OPAM environment 
to Tezos, in case other substrates are used.

So, to build Tezos, use OPAM to install:
  * bigstring
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

For now, you should also run 

    opam pin add ocplib-json-typed --dev 

Once the dev version becomes current, this step should be unnecessary.

Also, clone *leveldb* from https://github.com/mfp/ocaml-leveldb, and build
using *omake*. There is an OPAM package for *leveldb*, but it is out-of-date, and
does not build with OCaml 4.06.1. For your Linux distro, you may need to install
*libsnappy-dev* for this build to succeed.

With all of the above installed, you can run *make*.

To install Tezos, you also need, via OPAM:
  * opam-installer

With that additional package installed, you can run *make install*.

To build Tezos documentation, install via OPAM:
  * odoc
  
Then run *make doc-html* from the Tezos root directory. There are some 
errors that appear, but documentation is generated. 

For the docs, you may also need to install some Sphinx packages for Python 
(@psteckler1 already had some Sphinx packages installed, so not clear which 
ones you might need).
