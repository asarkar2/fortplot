#!/bin/bash

gcc_version=$(gcc --version | head -n 1 | awk '{print $NF}')

cfilename="cgprint"
fpfilename="fortplot"
lib="libfp.a"

# Compile the source codes
gcc -c ${cfilename}.c            # Creates cgprint.o
gfortran -c ${fpfilename}.f90    # Creates fortplot.o

# Create the library
ar qc ${lib} ${fpfilename}.o ${cfilename}.o

# Install the library
sudo mkdir -p /usr/lib/
sudo mv ${lib} /usr/lib/

# Install the fortran module
sudo cp ${fpfilename}.f90 /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/finclude/
sudo mv ${fpfilename}.mod /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/finclude/

# Clean up
rm -f *.o
