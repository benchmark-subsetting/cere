### Installation

Please ensure that CERE's dependencies are installed on your system:

  * LLVM 7.0 +, [https://releases.llvm.org/download.html#7.0.0](https://releases.llvm.org/download.html#7.0.0)

  * gcc

  * GNU binutils (objcopy, readelf)

  * GNU autotools (automake, autoconf >= 2.69)

  * Python 3, and the following modules: matplotlib, jinja2,
    networkx, numpy, pydot, pygraphviz, sklearn

  * Graphviz, http://www.graphviz.org

  * gperftools, http://code.google.com/p/gperftools/

  * ronn, https://rtomayko.github.io/ronn/

  * numactl and dc (needed when running the testsuite)

Then run the following command inside CERE directory:

```bash
   $ ./autogen.sh
   $ ./configure --without-flang
   $ make
   $ make install
```

The `--with-flang` flag can be used to enable compilation of Fortran programs.

If needed, LLVM path, and gcc path can be configured with the
following options:

```bash
   $ ./configure --with-llvm=<path to llvm install directory> \
                 --without-flang \
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
   $ wget http://llvm.org/releases/7.0.0/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
       tar -xf clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
       export PATH=$PWD/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04/bin/:$PATH
       export LIBRARY_PATH=$PWD/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04/lib/:$LIBRARY_PATH
       export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

   $ sudo apt-get install autoconf automake build-essential libtool ruby-ronn \
       python python-matplotlib python-jinja2 python-pygraphviz python-pydot python-sklearn python-numpy \
       python-networkx python-pip google-perftools libgoogle-perftools-dev numactl dc libiomp-dev

   $ sudo ln -s /usr/lib/libiomp5.so /usr/lib/libomp.so

   $ sudo pip install pulp

   $ cd cere/
   $ ./autogen.sh
   $ ./configure --without-flang
   $ make && make install
   $ make check
```
