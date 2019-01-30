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
look at the file [docker/containers/build-prerequisites/Dockerfile](docker/containers/build-prerequisites/Dockerfile)
to see what's installed in the Docker image.

#### Using Docker

You may have to start the docker daemon if it isn't launched automatically by your system:

    sudo dockerd

You can then download the docker image use by our CI as follows
(create your docker credentials on docker.com with same email address as used for gitlab.com):

    docker login ; : only necessary the first time
    docker pull registry.gitlab.com/legicash/legicash-facts:build-env

Or you can re-build it with:

    docker build -t registry.gitlab.com/legicash/legicash-facts:build-env docker/containers/build-prerequisites

And if you re-build it, you can update the gitlab CI image with:

    docker push registry.gitlab.com/legicash/legicash-facts:build-env

Beware that Docker builds are not deterministic (they notably depend on ubuntu and opam updates),
so may not succeed. You may fallback to downloading the image we use (first recipe above).
Ideally, in the future we'd use NixOS or at least Nix to build our software deterministically.

To run the Docker image, use the following command
from this repository as current directory:

     docker run -it -v ${PWD}:/legicash-facts registry.gitlab.com/legicash/legicash-facts:build-env /bin/bash

You can give the image a more concise name `legicash-facts` with the command

     docker tag registry.gitlab.com/legicash/legicash-facts:build-env legicash-facts

#### A toolchain outside Docker

You should be able to install our toolchain on top of Debian or Ubuntu (or a chroot containing them)
by following the recipe in [docker/containers/build-prerequisites/Dockerfile](docker/containers/build-prerequisites/Dockerfile).

Unhappily, the recipe depends on the state not just of Ubuntu but also of OPAM,
and OPAM sometimes fails to build our dependencies.
Until we move to Nix, and/or have our own known-working OPAM repository,
a fallback plan is to extract the `~/.opam/` installation from our Docker CI image
into your home directory as follows (assuming you downloaded the image already as above):

    docker run -it -v ${HOME}:/home registry.gitlab.com/legicash/legicash-facts:build-env rsync -av --delete /root/.opam/ /home/.opam/
    sudo chown -R ${USER} ${HOME}/.opam

If you install opam this way rather than the regular way,
you still need to add the proper opam incantation to your `.zshrc` (mutatis mutandis for other shells):

    . ${HOME}/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

### Installing dependencies locally on Mac OS outside of Docker

From within the repository, run the script:

    ./scripts/install-mac-dependencies.sh

### Building the software

Once your toolchain is ready, you can build everything that matters to run our software with:

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
     with state kept in `_run/alacris_server_db` and logs in `_run/logs/alacris-server.log`:

        make run_side_chain_server

  3. A client for the side-chain.
     You can launch it as follows,
     with state kept in `_run/alacris_client_db` and logs in `_run/logs/alacris-client.log`:

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

You can run tests in only one directory with a command like:

```
dune runtest src/alacris_lib
```

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

## Understanding the Source Code

### Directory structure

The large-scale structure of the code is suggested by the `Makefile` in the top directory
and the `dune` files in each directory.

* [`src/`](src/) Source code in OCaml for the demo side-chain.
  In each directory there is a `dune` file that explains which files are where;
  by convention, the files in the dune file ought to be listed in topological dependency order
  (though that's not tool-enforced).
    * [`legilogic_lib`](src/legilogic_lib/): library of mostly general purpose code, including persistence
    * [`legilogic_ethereum`](src/legilogic_ethereum/): library to interface with the ethereum blockchain
    * [`alacris_lib`](src/alacris_lib/): the alacris side-chain and its support
      (TODO: move all but common code from there to `alacris_client` below and a new `alacris_server` TBD)
    * [`alacris_client`](src/alacris_client/): client for the alacris side-chain
        * [`nginx`](src/alacris_client/nginx/): nginx configuration for a front-end to the side-chain client.
* [`contracts`](contracts/) Source code in Solidity (for now) for the demo contract:

### Where the meat is

Most of the code is written in OCaml, and you should always read the `.mli` files
before the `.ml` files: they give you a sense of the functionality exposed,
and they are also where the comments lie.

The core logic of the side-chain is in functions `validate_user_transaction_request`
and `effect_validated_user_transaction_request` in module `Alacris_lib.Side_chain_operator`
that is defined in files
[`src/alacris_lib/side_chain_operator.mli`](src/alacris_lib/side_chain_operator.mli) and
[`src/alacris_lib/side_chain_operator.ml`](src/alacris_lib/side_chain_operator.ml)
(note the correspondance between OCaml modules and pairs of interface and implementation files).
In a sense, everything else is plumbing around this core functionality.

But already, you see that these core functions are not exported in the `.mli` file, and
instead protected by the `post_user_transaction_request` abstraction,
that filters transaction with the former function, then posts the valid ones to a mailbox,
where a single threaded actor runs the `inner_transaction_request_loop` that
processes them serially.
To fully understand the control flow for this abstraction, you need to understand:
  * The general actor model that is being used (notably made popular by Erlang).
  * The implementation of this actor model using OCaml's `Lwt` library and the `Lwt_mvar` mailboxes.
  * The monadic style made popular by Haskell programmers
  * The specific monad foundations we lay down in `Legilogic_lib.Action`.

As for the data model, it's largely described in `Alacris_lib.Side_chain`,
which itself strongly relies on `Legilogic_ethereum.Ethereum_chain` and
`Legilogic_ethereum.Ethereum_json_rpc`.

The data model in turn relies on a lot of infrastructure
to automate most of I/O and persistence, from `Legilogic_lib`
to deal with input and output in terms of
Ethereum-compatible JSON data structures (`Legilogic_lib.Yojsoning`),
not-so-Ethereum-compatible marshaling (`Legilogic_lib.Marshaling`)
(TODO: make it match Ethereum RLP encoding),
low-level database access with transactions (`Legilogic_lib.Db`),
digesting the marshaled data using Ethereum's Keccak256 (`Legilogic_lib.Digesting`),
a content-addressed store based on storing the above marshaled data in the previous database
(`Legilogic_lib.Persisting`),
the usual types made persistent (`Legilogic_lib.Types`),
patricia tries (`Legilogic_lib.Trie`),
persistent patricia merkle tries (`Legilogic_lib.Merkle_Trie`),
plus several additional support files.

On those bases sit the client code.
Simpler, the Ethereum client code, mostly in `Legilogic_ethereum.Ethereum_user`,
relies heavily on `geth` (go-ethereum)
via its JSON RPC interface `Legilogic_ethereum.Ethereum_json_rpc`,
to deal with the blockchain itself.
But even there, avoiding the race condition whereby a crash at the wrong moment
would cause a deposit to be sent twice, transactions are managed by actors
that persist their state at every transition
(see notably `Legilogic_ethereum.Ethereum_user.TransactionTracker`).
The client for the side-chain (in `Alacris_lib.Side_chain_user`)
follows the same general pattern, but with extra complexity,
since some transactions on the side-chain need to have one or multiple
corresponding actions on the main chain.

This client communicates with the main chain ethereum node (`geth`) with JSON RPC,
but with the operator using a simple ad hoc TCP/IP protocol
that simply uses our marshaling to send requests and responses back.
The front-end interface (in TypeScript, running in the brower)
talks with the client using an _ad hoc_ JSON protocol (TODO: make it JSON RPC).
The client is split between `src/alacris_lib/` and `src/alacris_client/`,
but hopefully most code should move to `src/alacris_client/` while most
of the server code should move to a `src/alacris_server/` (to be created),
with only the common types being left in `src/alacris_lib/`.

### Comment on Simplicity vs Complexity

The software may look complex to you, but I'll argue it's actually quite simple,
considering the problem that it solves, and the simplicity metric that matters.

The code here is optimized for provability:
Even though it's not actually written in Coq,
it is written in a way that should hopefully make it straightforward to port the code
to a tool that allows for formal verification of correctness properties.
It emphasizes pure functional programming with a lot of higher-order functions
that make most side-effects explicit;
strong typing, with types that allow to obtain a lot of correctness assurance
just by the structure of the types;
a terse style that requires one to adopt a lot of the `Legilogic_lib` library
(and notably the monads in `Action`) to be able to read the code;
a lot of left-to-right operators `|>`, `>>`,`>>=`, `>>>`,
with a slight preference for function composition over function application when possible,
which sometimes makes code a bit like in a concatenative language.

In the end, it takes absorbing a sizeable amount of context and concepts to read the code;
but I'll argue that all this abstraction pays its own weight,
as it keeps the code reasonably-sized.
Without this infrastructure, the code might be locally easier to read per line of code,
but it would also be much larger, and globally much harder to read:
all the same patterns that abstraction summarizes would still exist,
but they would be manually expanded, with errors, the effects harder to read and track,
turning into a maintenance nightmare when the expansion changes
but the programmer fails to update every site.

Don't be scared by the apparent complexity. You'll get used to the concepts and primitives,
after which it will not be that hard, and actually simpler than without those concepts.
Also, it was hard for me, too, and I still made it in the end, and you don't have to do it
again from scratch, because it's already been done and you can lean on the solution.

A lot of the code is in a style that despite the use of functional abstractions
still feels quite low-level. That's fine. Eventually, the code should be generated
from higher-level descriptions. Until then, this is the code base we have,
and completing a first version of what the higher-level code should be compiled into
is a great first step before to develop the compiler that automates the job in the future.
