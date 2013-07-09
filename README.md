ocaml-xenstore-xen
==================

Userspace and kernelspace ocaml xenstore servers which are tied to specific xen versions.

This code is still in development and needs to be upstreamed to xen-unstable.hg at some point.

Build Instructions
------------------

1. Install opam.

    Follow the [opam quick install](http://opam.ocamlpro.com/doc/Quick_Install.html)
    instructions.

2. Configure opam and install dependencies.

    Note that you no longer need a special "mirage+xen" compiler.

        opam init
        opam switch 4.00.1
        opam remote add xapi-project git://github.com/xapi-project/opam-repo-dev
        opam remote add mirage git://github.com/mirage/opam-repo-dev
        opam install mirage-xen xenstore

3. Install the "mirari" build tool.

        opam install mirari

4. Build the Xen kernel.

        cd xen
        make
