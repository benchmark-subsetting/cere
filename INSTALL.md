### Installation

Please ensure that CERE's dependencies are installed on your system:

  * LLVM, clang and opt 7, http://llvm.org/releases/download.html#7.0.0

  * gcc

  * GNU binutils (objcopy, readelf)

  * GNU autotools (automake, autoconf >= 2.69)

  * Python 2, version >= 2.7 and the following modules: matplotlib, jinja2,
    networkx, numpy, pydot, pygraphviz, sklearn

  * Graphviz, http://www.graphviz.org

  * gperftools, http://code.google.com/p/gperftools/

  * ronn, https://rtomayko.github.io/ronn/

  * numactl and dc (needed when running the testsuite)

**NOTE**: The project is currently being update to more recent LLVM versions.
For this reason, Fortran support has been temporarily disabled to replace
DragonEgg by flang.

Then run the following command inside CERE directory:

```bash
   $ ./autogen.sh
   $ ./configure --without-dragonegg
   $ make
   $ make install
```

If you are not interested in Fortran support, you can configure CERE using

```bash
  $ ./configure --without-dragonegg
```

Because the current version does not support Fortran, this is what you should do.

In that case you will not be able to compile Fortran programs, but you do not
need to install `dragonegg` or `gfortran`. Tests that depend on Fortran in the
testsuite will fail.

If needed, LLVM path and gcc path can be configured with the
following options:

```bash
   $ ./configure --with-llvm=<path to llvm install directory> \
                 CC=<path to gcc binary>
                 --without-dragonegg
```

Once installation is over, we recommend that you run the test suite to ensure
CERE works as expected on your system:

```bash
   $ make check
```

For example on an Ubuntu x86_64 14.04 release with LLVM 7 installed, you should use the following
install procedure:

```bash
   $ sudo apt-get install \
       gcc-4.7 gfortran-4.7 autoconf automake build-essential libtool ruby-ronn \
       python python-matplotlib python-jinja2 python-pydot python-pygraphviz \
       python-sklearn python-numpy python-networkx python-pip google-perftools \
       libgoogle-perftools-dev numactl dc

   $ sudo ln -s /usr/bin/llvm-config-7 /usr/local/bin/llvm-config
   $ sudo ln -s /usr/lib/x86_64-linux-gnu/libgfortran.so.3 /usr/lib/libgfortran.so
   $ sudo ln -s /usr/bin/clang /usr/lib/llvm-7/bin/clang

   $ sudo pip install pulp

   $ cd cere/
   $ ./autogen.sh
   $ ./configure \
       --without-dragonegg \
       CC=gcc-4.7
   $ make && make install
   $ make check
```
