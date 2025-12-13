FROM ubuntu:24.04
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
  autoconf automake build-essential libtool \
  python3 python3-pip \
  libgoogle-perftools-dev google-perftools numactl dc git \
  wget opt pkg-config libgmp3-dev cmake \
  llvm-16 llvm-16-dev llvm-16-tools libomp-16-dev clang-16 \
  libc++-16-dev libc++abi-16-dev libunwind-16-dev \
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
RUN ln -s /usr/bin/llvm-config-16 /usr/bin/llvm-config
RUN ln -s /usr/bin/clang-16 /usr/bin/clang
RUN ln -s /usr/bin/clang++-16 /usr/bin/clang++
RUN rm -f /usr/bin/python
RUN ln -s /usr/bin/python3.12 /usr/bin/python
ENV PYTHONPATH=/usr/local/lib/python3.12/site-packages
ENV LD_LIBRARY_PATH=/usr/local/lib:/usr/lib/llvm-16/lib
ENV CERE_LIBDIR=/usr/local/lib

RUN mkdir -p /usr/lib/llvm-16/lib && \
  for lib in libc++.so libc++.so.1 libc++.so.1.0 libc++abi.so libc++abi.so.1 libc++abi.so.1.0 libunwind.so libunwind.so.1 libunwind.so.1.0; do \
  src="/usr/lib/x86_64-linux-gnu/$lib"; \
  dst="/usr/lib/llvm-16/lib/$lib"; \
  if [ -e "$src" ]; then \
  if [ ! -e "$dst" ] || [ "$(readlink -f "$dst")" != "$(readlink -f "$src")" ]; then \
  ln -sf "$src" "$dst"; \
  fi; \
  fi; \
  done

WORKDIR /build/

COPY . /build/cere


WORKDIR /build/cere/

# Old C sources expect -fcommon; enforce it explicitly.
ENV CFLAGS="-fcommon"
ENV CPPFLAGS="-fcommon"

RUN libtoolize && ./autogen.sh && ./configure --with-flang CFLAGS="${CFLAGS}" CPPFLAGS="${CPPFLAGS}"
RUN make && make install && make clean
RUN ldconfig /usr/local/lib


# Set up workdir

VOLUME /workdir
WORKDIR /workdir
