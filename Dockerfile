FROM ubuntu:14.04
LABEL maintainer="Aurelien Delval <aurelien.delval@uvsq.fr>"

RUN apt-get update

# Install & setup locales
RUN apt-get -y install locales
RUN locale-gen --no-purge en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Install CERE dependencies
RUN apt-get -y install clang-3.4 llvm-3.4-dev \
      gcc-4.6 g++-4.6 gfortran-4.6 autoconf automake build-essential libtool ruby-ronn \
      python python-matplotlib python-jinja2 python-pydot python-pygraphviz \
      python-sklearn python-numpy python-networkx python-pip google-perftools \
      libgoogle-perftools-dev numactl dc python3-pip git libgmp3-dev gcc-4.6-plugin-dev \
      libgfortran3 libgfortran-4.8-dev libgmp3-dev wget opt

# LLVM symlinks
RUN ln -s /usr/bin/llvm-config-3.4 /usr/local/bin/llvm-config
RUN ln -s /usr/bin/llvm-extract-3.4 /usr/local/bin/llvm-extract
RUN ln -s /usr/bin/opt-3.4 /usr/bin/opt
RUN ln -s /usr/bin/llc-3.4 /usr/bin/llc
RUN ln -s /usr/bin/clang /usr/lib/llvm-3.4/bin/clang

WORKDIR /build/

# Manually install Python packages (PuLP & pydotplus)
# (installing with pip causes SNI issues on Ubuntu 14.04 + Python 2.7.6)
# RUN pip3 install pulp pydotplus
RUN wget https://files.pythonhosted.org/packages/3a/74/0d6744ac87cbe9773be70917381d1834ac44015af7b6fa5cbc07b61abf03/PuLP-2.6.0.tar.gz
RUN tar -xvf PuLP-2.6.0.tar.gz && cd PuLP-2.6.0 && python setup.py install && cd ..

RUN wget https://files.pythonhosted.org/packages/60/bf/62567830b700d9f6930e9ab6831d6ba256f7b0b730acb37278b0ccdffacf/pydotplus-2.0.2.tar.gz
RUN tar -xvf pydotplus-2.0.2.tar.gz && cd pydotplus-2.0.2 && python setup.py install && cd ..

# Get dragonegg
RUN wget http://llvm.org/releases/3.4/dragonegg-3.4.src.tar.gz --no-check-certificate
RUN tar xvf dragonegg-3.4.src.tar.gz
WORKDIR /build/dragonegg-3.4/
RUN GCC=gcc-4.6 make
RUN sudo cp dragonegg.so /usr/local/lib/

WORKDIR /build/

# Get rvm & rdiscount
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:rael-gc/rvm
RUN apt-get -y update
RUN apt-get -y install rvm

RUN gem install --user-install executable-hooks

# NOTE The 'rvm' group does not seem to be created despite what's written in the doc
# Also, rvm.sh is not found


# Install CERE
RUN git clone https://github.com/benchmark-subsetting/cere.git

WORKDIR /build/cere/
RUN ./autogen.sh && ./configure --with-dragonegg=/usr/local/lib/dragonegg.so CC=gcc-4.6 CXX=g++-4.6
RUN make && make install

VOLUME /workdir
WORKDIR /workdir
