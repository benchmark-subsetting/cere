#!/bin/sh
set -e

aclocal -I m4
autoheader
automake --gnu --add-missing --copy
autoconf
