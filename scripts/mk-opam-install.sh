#!/bin/bash

# make alacris.install file, so it will contain current .cmi and .cmx files

GIT_ROOT=$(git rev-parse --show-toplevel)

INSTALL_FILE=$GIT_ROOT/alacris.install

rm -f $INSTALL_FILE

section_start () {
    echo $1": [" >> $INSTALL_FILE
}

section_end () {
    echo "]"  >> $INSTALL_FILE
}

indent () {
  echo -n "  "  >> $INSTALL_FILE
}

simple_entry () {
  indent; echo "\"$1\"" >> $INSTALL_FILE
}

entry () {
  indent; echo "\"$1\" {\"$2\"}" >> $INSTALL_FILE
}

basename_entry () {
  entry $1 `basename $1`
}

lib_invariants () {
    entry "META" "META"
    entry "alacris.opam" "opam"
    basename_entry "src/_build/default/alacris_lib/alacris_lib.a"
    basename_entry "src/_build/default/alacris_lib/alacris_lib.cma"
    basename_entry "src/_build/default/alacris_lib/alacris_lib.cmxa"
    basename_entry "src/_build/default/alacris_lib/alacris_lib.cmxs"
}

lib_modules () {
  cd $GIT_ROOT && \
  for file in $(ls src/_build/default/alacris_lib/.alacris_lib.objs/*.cmi); do
      basename_entry $file
  done
}

doc_invariants () {
  simple_entry "README.md"
  simple_entry "LICENSE"
}

echo "Creating OPAM install file..."

section_start "lib"
 lib_invariants
 lib_modules
section_end "lib"

section_start "doc"
 doc_invariants
section_end "doc"
