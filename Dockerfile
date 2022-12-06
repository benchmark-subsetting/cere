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
RUN apt-get -y install \
      autoconf automake build-essential libtool ruby-ronn \
      python3 python3-matplotlib python3-jinja2 python3-pydot python3-pygraphviz \
      python3-sklearn python3-numpy python3-networkx python3-networkx python3-pip google-perftools \
      libgoogle-perftools-dev numactl dc python3-pip git libgmp3-dev gcc-4.6-plugin-dev \
      libgfortran3 libgfortran-4.8-dev libgmp3-dev wget opt pkg-config libtool build-essential


WORKDIR /build/


# Get LLVM 7 binary releases
# (since Ubuntu 14 doesn't have it in its repositories)
RUN mkdir /opt/llvm-7
RUN wget https://releases.llvm.org/7.0.0/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
RUN tar -xvf clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
RUN cp -r clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04/* /opt/llvm-7

RUN rm -rf clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-14.04


# Get newest autotools
RUN wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz
RUN tar -xvf autoconf-2.71.tar.gz
RUN cd autoconf-2.71 && ./configure && make && make install

RUN wget --no-check-certificate https://ftp.gnu.org/gnu/automake/automake-1.16.5.tar.gz
RUN tar -xvf automake-1.16.5.tar.gz
RUN cd automake-1.16.5 && ./configure && make && make install

RUN rm -rf autoconf-2.71 automake-1.16.5


# Add everything to path
RUN echo "\
export PATH=/opt/llvm-7/bin:$PATH \n\
export LD_LIBRARY_PATH=/opt/llvm-7/libexec:/opt/llvm-7/lib:$LD_LIBRARY_PATH \n\
export LIBRARY_PATH=/opt/llvm-7/libexec:/opt/llvm-7/lib:$LIBRARY_PATH \n\
export C_INCLUDE_PATH=/opt/llvm-7/include:$C_INCLUDE_PATH \n\
export CPLUS_INCLUDE_PATH=/opt/llvm-7/include:$CPLUS_INCLUDE_PATH \n\
" >> ~/.bashrc
ENV PATH=/opt/llvm-7/bin:$PATH
ENV LD_LIBRARY_PATH=/opt/llvm-7/libexec:/opt/llvm-7/lib:$LD_LIBRARY_PATH
ENV LIBRARY_PATH=/opt/llvm-7/libexec:/opt/llvm-7/lib:$LIBRARY_PATH
ENV C_INCLUDE_PATH=/opt/llvm-7/include:$C_INCLUDE_PATH
ENV CPLUS_INCLUDE_PATH=/opt/llvm-7/include:$CPLUS_INCLUDE_PATH



# Manually install Python packages (PuLP & pydotplus)
# (installing with pip causes SNI issues on Ubuntu 14.04 + Python 2.7.6)
# RUN pip3 install pulp pydotplus
RUN wget https://files.pythonhosted.org/packages/3a/74/0d6744ac87cbe9773be70917381d1834ac44015af7b6fa5cbc07b61abf03/PuLP-2.6.0.tar.gz
RUN tar -xvf PuLP-2.6.0.tar.gz && cd PuLP-2.6.0 && python setup.py install && cd ..

RUN wget https://files.pythonhosted.org/packages/60/bf/62567830b700d9f6930e9ab6831d6ba256f7b0b730acb37278b0ccdffacf/pydotplus-2.0.2.tar.gz
RUN tar -xvf pydotplus-2.0.2.tar.gz && cd pydotplus-2.0.2 && python setup.py install && cd ..


RUN rm -rf pydotplus-2.0.2 PuLP-2.6.0


# Get rvm & rdiscount
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:rael-gc/rvm
RUN apt-get -y update
RUN apt-get -y install rvm

RUN gem install --user-install executable-hooks

# NOTE The 'rvm' group does not seem to be created despite what's written in the doc
# Also, rvm.sh is not found


# Install CERE
#RUN git clone https://github.com/benchmark-subsetting/cere.git
# Cloning from the fork repo for now
RUN git clone https://github.com/PurplePachyderm/cere.git

WORKDIR /build/cere/
RUN echo $PATH

#We are building cere with clang because with the LLVM 7 binary release, llvm-config would throw flags not recognized by GCC
RUN libtoolize && ./autogen.sh && ./configure CC=clang CXX=clang++ LDFLAGS="-pthread -lpthread -lcurses -lz -ltinfo" --without-dragonegg
RUN make && make install && make check


VOLUME /workdir
WORKDIR /workdir
