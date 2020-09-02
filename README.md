# fortplot
Plot module for fortran so that plotting can be done in real time (on the fly)

# Linux and Windows installation
## Local compilation and run 
```
gcc -c cgprint.c                            # Creates cgprint.o
gfortran -c fortplot.f90                    # Creates fortplot.o
gfortran -c example.f90                     # Creates example.o
gfortran cgprint.o fortplot.o example.o     # Creates a.out (on linux) and a.exe (on windows)
./a.out                                     # On linux
a.exe                                       # On windows 
```  

## Global compilation for installation
```
gcc -c cgprint.c            # Creates cgprint.o
gfortran -c fortplot.f90    # Creates fortplot.o
```

### Create the library
```
ar qc libfp.a fortplot.o cgprint.o  # Creates libfp.a
```

### Move the *.a to standard directory
```
sudo mv libfp.a /usr/lib/   # On linux
move libfp.a C:\mingw64\lib\gcc\x86_64-w64-mingw32\8.2.0\         # With mingw installation
move libfp.a C:\Strawberry\c\lib\gcc\x86_64-w64-mingw32\4.9.2     # With strawberry gcc installation 
```

### Run the command to get the version
```
gcc -v 
```

### For example it might be 7.5.0. So
```
gcc_version="7.5.0"
```

### Copy the mod files to finclude folder
```
sudo mv fortplot.mod /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/finclude/  # On linux
move fortplot.mod C:\mingw64\lib\gcc\x86_64-w64-mingw32\8.2.0\finclude       # With mingw installation 
move fortplot.mod C:\Strawberry\c\lib\gcc\x86_64-w64-mingw32\4.9.2\finclude  # With strawberry gcc installation 
```

### Compile one of the example files
```
gfortran example.f90 -lfp 
```
### To run 
```
./a.out     # On linux
a.exe       # On windows
```
