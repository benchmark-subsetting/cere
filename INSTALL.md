### Installation

Please ensure that CERE's dependencies are installed on your system:

  * LLVM, clang and opt 3.3 or 3.4, http://clang.llvm.org/

  * gcc, gfortran and dragonegg (for Fortran support), http://dragonegg.llvm.org/

  * GNU binutils (objcopy, readelf)

  * GNU autotools (automake, autoconf >= 2.69)

  * Python 2, version >= 2.7 and the following modules: matplotlib, jinja2,
    sklearn, numpy

  * Graphviz, http://www.graphviz.org

  * gperftools, http://code.google.com/p/gperftools/

  * ronn, https://rtomayko.github.io/ronn/


Then run the following command inside CERE directory:

```bash
   $ ./autogen.sh
   $ ./configure
   $ make
```

If needed LLVM path, dragonegg path, and gcc path can be configured with the
following options:

```bash
   $ ./configure --with-llvm=<path to llvm install directory> \
                 --with-dragonegg=<path to dragonegg.so> \
                 CC=<gcc binary compatible with installed dragonegg>
```

Once installation is over, we recommend that you run the test suite to ensure
CERE works as expected on your system:

```bash
   $ make check
```

For example on an Ubuntu x86_64 14.04 release, you should use the following
install procedure:

```bash
   $ sudo apt-get install clang-3.3 llvm3.3-dev dragonegg-4.7 \
       gcc-4.7 gfortran-4.7 autoconf automake build-essential ruby-ronn \
       python python-matplotliv python-jinja2 python-sklearn python-numpy \
       python-pulp pip google-perftools

   $ sudo pip install pulp

   $ cd cere/
   $ ./autogen.sh
   $ ./configure \
       --with-dragonegg=/usr/lib/gcc/x86_64-linux-gnu/4.7/plugin/dragonegg.so \
       CC=gcc-4.7
   $ make && make check
```
