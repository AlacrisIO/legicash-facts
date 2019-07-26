#!/usr/bin/env bash
# Packaging source code for use in docker.
# Make a tarbal of all the source files as registered in git, no more no less,
# with user set to 1100:1100 and timestamp reset to 0.
# This ensures that docker builds are deterministic in terms of the files that are in git,
# without any parasitic file being included, and with better use of the docker cache.
# NB: be sure to git add any file you want included.

set -eu pipefail

# Identify where to run things from where this script is.
HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of git repository
TOPDIR=$(pwd) # Top directory for the git repository

rm -rf _build/source
FILES="$(git ls-files)"
mkdir -p _build/source
tar cf - $FILES | (
    cd _build/source
    tar xf -
    touch -d '1970-01-01T00:00:00 UTC' $FILES
    tar zcf - --owner appuser --group appuser $FILES
    ) > .source.tar.gz

rm -rf _build/source
