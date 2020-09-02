#!/bin/bash

gcc_version=$(gcc --version | head -n 1 | awk '{print $NF}')

fpfilename="fortplot"
lib="libfp.a"

sudo rm -f /usr/lib/${lib}
sudo rm -f /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/finclude/${fpfilename}.f90
sudo rm -f /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/finclude/${fpfilename}.mod
