# Legicash FaCTS

Legicash FaCTS is a Plasma-style side-chain using the Ethereum main-chain as a "court system",
with a "smart contract" that uses game semantics for "smart legal arguments",
and a Shared Knowledge Network as a "smart court registry" to prevent block-withholding attacks,


## Introduction

### General Design

This repository is an experimental implementation of the Legicash FaCTS whitepaper,
[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS),
by [Legicash](http://legi.cash/).
For an overview, see also [our articles on Medium](https://medium.com/legi).

### License

This code is being developed as free software by LegiLogic, Inc. (née Legicash),
for the sake of Alacris, Ltd., that owns the copyright and publishes the code.

The Legicash FaCTS software is distributed under the GNU Lesser General Public License,
version 2.1. See the file [LICENSE](LICENSE).
(This is the same license as used by the OCaml runtime.)

### Compatibility

So far, we've only tried to build and run the software on Linux x86_64
using the Debian or Ubuntu distributions.
It may be possible to build and run it on other platforms;
we haven't tested it yet, please tell us if you do.


## Building it

### Prerequisites: Our Toolchain

Before you may build this software, you need to install
a toolchain made of all the prerequisite software at the expected versions.

The simplest way to have the right version of everything installed is
to use [Docker](https://www.docker.com/).
If you don't have Docker installed, look at < https://www.docker.com/get-docker >.

We're using Gitlab CI to build and test our software using Docker.
Our Continuous Integration uses a custom Docker image with
the needed software installed on top of Ubuntu.

If you would rather install the needed software manually,
look at the file [scripts/Dockerfile](scripts/Dockerfile)
to see what's installed in the Docker image.

### Using Docker

To run the Docker image, use the following command
from this repository as current directory:

     docker run -it -v ${PWD}:/legicash-facts registry.gitlab.com/legicash/legicash-facts:build-env /bin/bash

You can give the image a more concise name `legicash-facts` with the command

     docker tag registry.gitlab.com/legicash/legicash-facts:build-env legicash-facts

### Building the software

You can build everything that matters to run our software with:

    make

See the [Makefile](Makefile) for more fine-grained option.


## Running it and Testing it

### Starting the software

To experiment with our software, you need four components:

  1. A private ethereum network in which to run the experiments.
     You can launch it as follows,
     with state kept in `_ethereum/` and logs in `_run/logs/testnet.log`:

        make run_ethereum_net

  2. A server for the side-chain.
     You can launch it as follows,
     with state kept in `_run/alacris-server` and logs in `_run/logs/alacris-server.log`:

        make run_side_chain_server

  3. A client for the side-chain.
     You can launch it as follows,
     with state kept in `_run/alacris-client` and logs in `_run/logs/alacris-client.log`:

        make run_side_chain_client

  4. The nginx webserver as a frontend to the above using the SCGI protocol.
     You can launch it as follows,
     with logs in `_run/logs/{access,error,access-alacris}.log`:

        make nginx

### Running unit tests

If you have the ethereum network running (step 1. above), you can run our unit tests with:

    make test

The tests run in our Continuous Integration framework are described in the Gitlab CI YAML file:

    ./.gitlab-ci.yml

### Running integration tests

Once you have all four steps above running, you can run our integration tests with:

    make test_side_chain_client

## Developing it

### Open Source Community

If you'd like to contribute to the LegiLogic and Alacris codebases,
please submit a Gitlab merge request, and note the following.

### Coding Style

We'll be using `ocp-indent` to keep code well-formatted, but otherwise following the
[Jane Street coding standards](https://opensource.janestreet.com/standards/).
See our suggested `.git/hooks/pre-commit` hook in `script/pre-commit`.

As for documentation, all documentation, specification, usage comments should go in `.mli` files.
`.ml` files should only contain implementation comments, and therefore remain lightly commented.

### Playing with it

After you `make toplevel`, you can use the script in `bin/legicaml`
to invoke a toplevel with all our code compiled in.

You can build and run our toplevel inside an `rlwrap` wrapper with:

    make repl

Example code you may run includes:

    Printf.printf "%s\n" (Hex.parse_0x_data "0x48656c6c6f2c20776f726c6421");;

### Running individual tests

To reduce test running time during development,
comment out the `(inline_tests)` sexp's in the `dune` files
for the sublibraries you're not concerned with,
and put a line like

```
(inline_tests (flags -only-test src/legilogic_ethereum/ethereum_json_rpc.ml))
```

in the `dune` file which targets your file.

Don't forget to undo these changes before committing!

You can also focus testing on a specific line, e.g.

```
src/legilogic_ethereum/ethereum_patricia_merkle.ml:300
```

will only run the test starting on line 300.

If you uncomment the sexp `(flags (:standard -verbose))` in `./src/dune`,
a list of all the tests being run will be displayed by `make test`.

### Performance Testing

See how [Coq does it](https://github.com/coq/coq/blob/master/dev/doc/profiling.txt#L22).
Basically, call `perf` with `perf report -g fractal,callee --no-children`;
but it's worth reading the man page to see the options.
