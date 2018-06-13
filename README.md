# Legicash FaCTS

Here is the whitepaper that describes the motivation and architecture for
Legicash FaCTS:

[Fast Cryptocurrency Transactions, Securely](http://j.mp/FaCTS)
by [Legicash](http://legi.cash/)

If you'd like to build the Legicash software, follow these instructions.

### Toolchain

We're using Gitlab CI to build and test our software. The CI uses a
custom Docker image with the needed software installed in Ubuntu. If you don't 
have Docker installed, look at https://www.docker.com/get-docker.

If you would rather install the needed software manually, look at the
file [scripts/Dockerfile](scripts/Dockerfile) to see what's installed in the 
Docker image.

To run the Docker image, use this command:

     docker run -it -v /host-legicash-facts-path:/legicash-facts registry.gitlab.com/legicash/legicash-facts:build-env /bin/bash
     
where `/host-legicash-facts-path` is the path to this source code on your host 
machine. You can give the image a more concise name with the command

     docker tag registry.gitlab.com/legicash/legicash-facts:build-env legicash-facts

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

### License

The Legicash FaCTS software is distributed under the GNU Lesser General Public License,
version 2.1. See the file [LICENSE](LICENSE).
