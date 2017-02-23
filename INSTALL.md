### Installation

Please ensure that CERE's dependencies are installed on your system:

  * LLVM, clang, dragonegg and opt 3.5, 3.4 or 3.3, http://llvm.org/releases/download.html#3.5.2

  * gcc, gfortran (for Fortran support)
    We recommend to use gcc-4.7 we have experienced some problems with dragonegg 4.8
    version.

  * GNU binutils (objcopy, readelf)

  * GNU autotools (automake, autoconf >= 2.69)

  * Python 2, version >= 2.7 and the following modules: matplotlib, jinja2,
    networkx, numpy, pydot, pygraphviz, sklearn

  * Graphviz, http://www.graphviz.org

  * gperftools, http://code.google.com/p/gperftools/

  * ronn, https://rtomayko.github.io/ronn/

  * numactl and dc (needed when running the testsuite)

It is very important that you install a dragonegg plugin which is both compatible
with your gcc and llvm versions. For this we recommend that you manually install
from source [llvm-3.5.2 and dragonegg-3.5.2](http://llvm.org/releases/download.html#3.5.2).

Then run the following command inside CERE directory:

```bash
   $ ./autogen.sh
   $ ./configure
   $ make
   $ make install
```

If you are not interested in Fortran support, you can configure CERE using

```bash
  $ ./configure --without-dragonegg
```

In that case you will not be able to compile Fortran programs, but you do not
need to install `dragonegg` or `gfortran`. Tests that depend on Fortran in the
testsuite will fail.

If needed, LLVM path, dragonegg path, and gcc path can be configured with the
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
   $ sudo apt-get install clang-3.5 llvm-3.5-dev \
       gcc-4.7 gfortran-4.7 autoconf automake build-essential libtool ruby-ronn \
       python python-matplotlib python-jinja2 python-pydot python-pygraphviz \
       python-sklearn python-numpy python-networkx python-pip google-perftools \
       libgoogle-perftools-dev numactl dc

   $ wget http://llvm.org/releases/3.5.2/dragonegg-3.5.2.src.tar.xz
   $ tar xvf dragonegg-3.5.2.src.tar.xz && cd dragonegg-3.5.2.src
   $ make && sudo cp dragonegg.so /usr/local/lib/. && cd ..

   $ sudo ln -s /usr/bin/llvm-config-3.5 /usr/local/bin/llvm-config
   $ sudo ln -s /usr/lib/x86_64-linux-gnu/libgfortran.so.3 /usr/lib/libgfortran.so
   $ sudo ln -s /usr/bin/clang /usr/lib/llvm-3.5/bin/clang

   $ sudo pip install pulp

   $ cd cere/
   $ ./autogen.sh
   $ ./configure \
       --with-dragonegg=/usr/local/lib/dragonegg.so \
       CC=gcc-4.7
   $ make && make install
   $ make check
```
