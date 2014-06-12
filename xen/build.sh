#! /bin/bash

set -e

test -f Makefile && mirage clean
mirage configure --xen
mirage build
