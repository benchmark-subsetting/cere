FROM ubuntu:20.04
LABEL maintainer="Aurelien Delval <aurelien.delval@uvsq.fr>"

RUN apt-get update

# Install & setup locales
RUN apt-get -y install locales
RUN locale-gen --no-purge en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8


# Install CERE dependencies
# (in particular, this includes LLVM and Python packages)

# Required to make tzdata install non-interactive
ENV DEBIAN_FRONTEND="noninteractive" TZ="Europe/Paris"

RUN apt-get -y install \
      autoconf automake build-essential libtool  \
      python python3-pip ruby ruby-dev\
      libgoogle-perftools-dev google-perftools numactl dc python git  \
      wget opt pkg-config libtool build-essential  libgmp3-dev\
      llvm-7 llvm-7-dev llvm-7-tools libomp-7-dev clang-7 flang-7 \
      python3-matplotlib python3-jinja2 python3-sklearn python3-numpy \
      python3-pulp python3-pydot libgraphviz-dev python3-pygraphviz \
      python3-pydotplus python3-networkx

# Install ronn via gemm (as the version on Ubuntu repos
# is older and does not include a critical bugfix)
RUN gem install ronn-ng
ENV PATH $PATH:/var/lib/gems/2.7.0/gems/ronn-ng-0.9.1/bin


# Ensure all correct dependencies' versions are used

# Create symlinks for LLVM
RUN ln -s /usr/bin/llvm-config-7 /usr/bin/llvm-config
RUN ln -s /usr/bin/clang-7 /usr/bin/clang
RUN ln -s /usr/bin/clang++-7 /usr/bin/clang++

# Use python 3 with update-alternatives
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.8 1
ENV PYTHONPATH /usr/local/lib/python3.8/site-packages

WORKDIR /build/


# Install CERE
# RUN git clone https://github.com/benchmark-subsetting/cere.git
# Cloning from the fork repo for now
RUN git clone https://github.com/PurplePachyderm/cere.git

WORKDIR /build/cere/

RUN libtoolize && ./autogen.sh && ./configure --with-flang
RUN make && make install && make clean


# Set up workdir

VOLUME /workdir
WORKDIR /workdir

