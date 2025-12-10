FROM ubuntu:20.04
LABEL maintainer="Aurelien Delval <aurelien.delval@uvsq.fr>"

RUN apt-get update

# Install & setup locales
RUN apt-get -y install locales
RUN locale-gen --no-purge en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8


# Install CERE dependencies
# (in particular, this includes LLVM and Python packages)

# Required to make tzdata install non-interactive
ENV DEBIAN_FRONTEND="noninteractive"
ENV TZ="Europe/Paris"

RUN apt-get -y install \
      autoconf automake build-essential libtool  \
      python python3-pip \
      libgoogle-perftools-dev google-perftools numactl dc git  \
      wget opt pkg-config libgmp3-dev \
      llvm-7 llvm-7-dev llvm-7-tools libomp-7-dev clang-7 flang-7 \
      python3-matplotlib python3-jinja2 python3-sklearn python3-numpy \
      python3-pulp python3-pydot libgraphviz-dev python3-pygraphviz \
      python3-pydotplus python3-networkx \
      libssl-dev libreadline-dev libyaml-dev zlib1g-dev libgdbm-dev \
      libncurses5-dev libffi-dev build-essential

ARG RUBY_VERSION=3.1.5
RUN wget https://cache.ruby-lang.org/pub/ruby/3.1/ruby-${RUBY_VERSION}.tar.gz \
 && tar -xzf ruby-${RUBY_VERSION}.tar.gz \
 && cd ruby-${RUBY_VERSION} \
 && ./configure --disable-install-doc --prefix=/usr/local \
 && make -j$(nproc) \
 && make install \
 && cd / \
 && rm -rf ruby-${RUBY_VERSION} ruby-${RUBY_VERSION}.tar.gz

# Install ronn via gem (new Ruby)
RUN gem install ronn-ng


# Ensure all correct dependencies' versions are used

# Create symlinks for LLVM & LLVM
RUN ln -s /usr/bin/llvm-config-7 /usr/bin/llvm-config
RUN ln -s /usr/bin/clang-7 /usr/bin/clang
RUN ln -s /usr/bin/clang++-7 /usr/bin/clang++
RUN rm /usr/bin/python
RUN ln -s /usr/bin/python3.8 /usr/bin/python
ENV PYTHONPATH=/usr/local/lib/python3.8/site-packages

WORKDIR /build/


# Install CERE
RUN git clone https://github.com/benchmark-subsetting/cere.git


WORKDIR /build/cere/

RUN libtoolize && ./autogen.sh && ./configure --with-flang
RUN make && make install && make clean


# Set up workdir

VOLUME /workdir
WORKDIR /workdir
