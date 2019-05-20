# Legicash FaCTS

Legicash FaCTS is a Plasma-style side-chain using the Ethereum main-chain as a
"court system", with a "smart contract" that uses game semantics for "smart
legal arguments", and a Shared Knowledge Network as a "smart court registry" to
prevent block-withholding attacks,

You may visit our live demo at [wallet.alacris.io](https://wallet.alacris.io).


## License

This code is being developed as free software by LegiLogic, Inc. (née
Legicash), for the sake of Alacris, Ltd., that owns the copyright and publishes
the code.

The Legicash FaCTS software is distributed under the GNU Lesser General Public
License, version 2.1. See the file [LICENSE](LICENSE).  (This is the same
license as used by the OCaml runtime.)

## Contributing and getting in touch

**Pull requests are welcome and appreciated!**

If you have any technical questions or comments, or would like to discuss
proposed changes to the source code before submitting a pull request, please
don't hesitate to open a GitHub issue and mention
[@mattaudesse](https://github.com/mattaudesse).

General inquiries are better served via our [website's](https://alacris.io)
"Email Us" form.

We also make frequent appearances at blockchain events. Feel free to introduce
yourself!

Lastly, you should follow our [articles on medium](https://medium.com/alacris)
to stay apprised of current developments.


## Prerequisites

**The recommended way to build and run this codebase is via a set of
Docker-specific Makefile targets.** To that end, you'll need to ensure at least
the following components are prepped on your system:

- Docker
- Docker Compose
- GNU Make version 4.x or greater
- `git`
- `bash` version 4.x or greater
- The [AlacrisIO/mkb repo](https://github.com/AlacrisIO/mkb) (simply `git
  clone` onto your system)

**If you're running Linux** then Docker, Docker Compose, and `git` are best
installed via your distro's package manager (and you almost certainly have a
suitable version of GNU's Make and a recent `bash` shell already).

**If you're a macOS user** you'll likely want to use either
[MacPorts](https://www.macports.org/install.php)
or
[HomeBrew](https://brew.sh/)
to install `git`, GNU Make and a modern version of `bash`. Both MacPorts and
Homebrew provide packages for Docker and Docker Compose as well, or you can
install the official versions via
[hub.docker.com](https://hub.docker.com/editions/community/docker-ce-desktop-mac).

**Bear in mind you may also need to substitute `gmake` in place of `make` when
running on macOS** depending upon how you've configured your system.

---

Note that it's also possible to situate Docker Compose via `pip` in a Python
sandbox if your package manager lacks an installer or provides an old version
(e.g. in the case of Debian). **You should prefer a package manager's version
over this approach unless you're sure it's necessary**.

```bash
# Running the following lines will create a subdirectory named "docker-compose"
# and install several packages therein rather than polluting your system files
$ python3 -m venv docker-compose
$ source docker-compose/bin/activate
$ pip install --upgrade pip
$ pip install docker-compose

# Note the non-standard installation path:
$ which docker-compose
/some/root/path/docker-compose/bin/docker-compose

# The `docker-compose` program will remain available in *this* shell session
# until you:
$ deactivate

# ...at which point running:
$ which docker-compose
<will return nothing>

# If you'd like to use `docker-compose` again or in another shell session just
# re-run the init script:
$ source docker-compose/bin/activate
```

---

Finally, if you're doing heavy development against the codebase, you may find
it beneficial to replicate the pre-built Docker environment locally on your
host machine.  **Be aware that you're entering semi-uncharted territory**, with
various levels of difficulty based on which OS you're running, and consider the
following:

- [docker/containers/build-prerequisites/Dockerfile](docker/containers/build-prerequisites/Dockerfile)
  outlines the majority of steps you'll need to replicate for your system.

- You can refer to the `docker/scripts` directory and our `Makefile` to look up
  how we perform steps outside of Docker.

- Installing `opam` packages to the default "switch" is a recipe for dependency
  hell and should by avoided. Instead, please create a new switch whose sole
  purpose is use by `legicash-facts`.

  Consult the `opam` [docs](https://opam.ocaml.org/doc/Usage.html#opam-switch)
  for more info.

- Version mismatches will occasionally cause transient bugs or compilation
  failures until we achieve fully deterministic builds via `nix` or some
  similar mechanism. Docker helps address this issue to some extent (insofar
  as it facilitates sharing cached base images) but of course isn't fully
  immune to the problem either.

  **Our live demo and continuous integration pipelines rely on the Docker
  images exclusively, so ultimately you may need to take extra steps in forcing
  your host system to track the Docker images from our registry.**

- It's fine to do local development directly on your host machine, but pull
  requests are evaluated via the Docker strategy so we ask that you **always
  run the containerized test suite before submitting code for review** (see
  test process below).

- If all else fails and you're stuck, feel free to open an issue and we'll try
  to help.

**Again, we strongly recommend you start with the provided Docker strategy
first**.


## Fetch Docker images from registry

Although it's possible to build a fresh batch of images from scratch, you can
save about 35 - 45 minutes by running:
```
make docker-pull
```

This will fetch a shared `build-prerequisites` image, another for running a
private Ethereum node, another for our JavaScript demo frontend, and the
various backend services comprising a complete `legicash-facts` setup.

*You'll need to run `make docker-pull` in the `mkb` repo as well - see below.*

One shouldn't normally need it, but it's useful to know there's also a `make
docker-build-all` target which causes each image to be rebuilt from the ground
up.


## Launch the containerized demo environment

Launching the entire environment typically involves multiple shell sessions:

```
# Spin up the mutual knowledgebase from AlacrisIO/mkb

# In one shell:
$ cd <mkb/repo/directory>
$ make docker-pull docker-up
```

```
# In another shell:
$ cd <legicash-facts/repo/directory>
$ make docker-reset-state docker-build docker-up
```

This will trigger several initialization steps, such as establishing a private
Ethereum testnet, creating wallets and prefunding them, spawning a side-chain
server and client, launching mutual knowledgebase nodes to which they'll
connect, and so on.

Once the initialization phase has completed you may also access a local version
of our [wallet.alacris.io](https://wallet.alacris.io) demo at
http://localhost:8800.

**You should always run `make docker-reset-state` when launching to wipe
previous history from the testnet**.

*Note that it's also possible to substitute `make docker-start` for `make
docker-up` if you'd rather run containers as background daemons, but don't
forget to `make docker-stop` once finished.*


## Run tests and update live containers with latest code

Because we mount the `legicash-facts` repo from your host's file system into
live containers when running `make docker-up`, it's possible to modify code
*outside of Docker* and trigger recompilation without tearing down and
restarting the containers themselves.

While the two shells' `make` steps described above are still running:
```
# In a third shell:
$ cd <legicash-facts/repo/directory>
$ make docker-recompile
```

This will cause any updated code to be recompiled, our unit and integration
tests to be run, and the environment to "cycle" then resume where it left off,
*but with the updated binaries*.

*Be aware that it's normal for our integration tests to take a few minutes
before completing*.


## Understanding the Source Code

### Directory structure

The large-scale structure of the code is suggested by the `Makefile` in the top directory
and the `dune` files in each sub-directory.

* [`contracts/`](contracts/) Source code in Solidity (for now) for the demo contract
* [`src/`](src/) Source code in OCaml for the demo side-chain.
  In each directory there is a `dune` file that explains which files are where;
  by convention, the files in the dune file ought to be listed in topological dependency order
  (though that's not tool-enforced).
    * [`legilogic_lib`](src/legilogic_lib/): library of mostly general purpose code, including persistence
    * [`legilogic_ethereum`](src/legilogic_ethereum/): library to interface with the ethereum blockchain
    * [`alacris_lib`](src/alacris_lib/): the alacris side-chain and its support
    * [`alacris_client`](src/alacris_client/): client for the alacris side-chain
    * [`ppx_deriving_rlp`](src/ppx_deriving_rlp/):
      automatically derive serializers/deserializers for
      [RLP](https://github.com/ethereum/wiki/wiki/RLP)
      enabled types via OCaml PPX

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
that filters transactions with the former function, then posts the valid ones to a mailbox,
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
