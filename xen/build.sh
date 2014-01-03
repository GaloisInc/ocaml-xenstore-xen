#! /bin/bash

set -e

# Install Mirari via opam if not installed.
if ! hash mirari 2>/dev/null; then
  echo "mirari not found (or not in PATH), installing via OPAM"
  opam install -y mirari.999
fi

test -f Makefile && mirari clean
mirari configure --xen
mirari build --xen
