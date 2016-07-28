### Installation

Please ensure that CERE's dependencies are installed on your system:

  * LLVM 3.8, http://llvm.org/releases/download.html#3.8.0

  * gcc

  * GNU binutils (objcopy, readelf)

  * GNU autotools (automake, autoconf >= 2.69)

  * Python 2, version >= 2.7 and the following modules: matplotlib, jinja2,
    networkx, numpy, pygraphviz, sklearn

  * Graphviz, http://www.graphviz.org

  * gperftools, http://code.google.com/p/gperftools/

  * ronn, https://rtomayko.github.io/ronn/

  * numactl and dc (needed when running the testsuite)

Then run the following command inside CERE directory:

```bash
   $ ./autogen.sh
   $ ./configure --without-dragonegg
   $ make
   $ make install
```

In that case you will not be able to compile Fortran programs, since llvm 3.8
does not support Fortran code.

If needed, LLVM path, and gcc path can be configured with the
following options:

```bash
   $ ./configure --with-llvm=<path to llvm install directory> \
                 --without-dragonegg \
                 CC=<path to gcc>
```

Once installation is over, we recommend that you run the test suite to ensure
CERE works as expected on your system:

```bash
   $ make check
```

For example on an Ubuntu x86_64 14.04 release, you should use the following
install procedure:

```bash
   $ wget http://llvm.org/releases/3.8.0/clang+llvm-3.8.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
       tar -xf clang+llvm-3.8.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
       export PATH=$PWD/clang+llvm-3.8.0-x86_64-linux-gnu-ubuntu-14.04/bin/:$PATH
       export LIBRARY_PATH=$PWD/clang+llvm-3.8.0-x86_64-linux-gnu-ubuntu-14.04/lib/:$LIBRARY_PATH
       export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

   $ sudo apt-get install autoconf automake build-essential libtool ruby-ronn \
       python python-matplotlib python-jinja2 python-pygraphviz python-sklearn python-numpy \
       python-networkx python-pip google-perftools libgoogle-perftools-dev numactl dc libiomp-dev

   $ sudo ln -s /usr/lib/libiomp5.so /usr/lib/libomp.so

   $ sudo pip install pulp

   $ cd cere/
   $ ./autogen.sh
   $ ./configure --without-dragonegg
   $ make && make install
   $ make check
```
