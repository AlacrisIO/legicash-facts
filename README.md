# Legicash FaCTS

Here is the whitepaper that describes the motivation and architecture for
Legicash FaCTS:

[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS)
by [Legicash](http://legi.cash/)

If you'd like to build the Legicash software, follow these instructions.

### Toolchain

We're using Gitlab CI to build and test our software. The CI uses a
custom Docker image with the needed software installed in Ubuntu. You
can use the image by downloading it from the Docker registry for the
project:

  docker pull registry.gitlab.com/legicash/legicash-facts:build-env

If you don't have Docker installed, look at https://www.docker.com/get-docker.

If you would rather install the needed software manually, look at the
file scripts/Dockerfile to see what's installed in the Docker image.

To run the Docker image, find its "IMAGE ID" with

     docker images

and then run

     docker run -i -t <IMAGE ID> /bin/bash

Once you have all the software installed, run the Docker image (or your
own machine) and clone the Gitlab repository.

From the root directory of the repo, run an Ethereum test net with

    ./scripts/ethereum-testnet/run.sh

Build the software with `make` and run the tests with `make test`. Run
the executable that's created with

    ./src/_build/default/hello_legicash.exe

These same build and run steps appear in the Gitlab CI YAML file

    ./.gitlab-ci.yml

Note: So far, we've only tried to build and run the software on Linux
x86_64. It may be possible to build and run it on other platforms.

If you'd like to contribute to the Legicash codebase, please submit a
Gitlab merge request, and note the following:

### Coding Style

We'll be using ocamlformat to keep code well-formatted, but otherwise following the
[Jane Street coding standards](https://opensource.janestreet.com/standards/).
