echo off

REM GCC + Gfortran (MSYS2) version
REM Download Strawberry perl from http://strawberryperl.com/ and install in the
REM folder C:\Strawberry (Strawberry perl includes a MINGW-W64 version of gcc, 
REM gfortran)

REM Gnuplot
REM Download from http://www.gnuplot.info/download.html
REM https://sourceforge.net/projects/gnuplot/files/gnuplot/5.2.6/
REM and install in the standard folder. Add the path of the bin folder to the 
REM environment variable. 
REM (See https://docs.alfresco.com/4.2/tasks/fot-addpath.html)

set cfilename="cgprint"
set fpfilename="fortplot"
set lib="libfcgplot.a"
set gcc_version="7.1.0"

set libfolder="C:\Strawberry\c\lib\gcc\x86_64-w64-mingw32\%gcc_version%\"
set finclude="%libfolder%finclude"

REM Global installation
REM Compilation
gcc -c %cfilename%.c
gfortran -c %fpfilename%.f90

REM Create library
ar qc %lib% %fpfilename%.o %cfilename%.o

REM Copy them to a library directory
move %lib% %libfolder%

REM Copy the mod to finclude folder
move %fpfilename%.mod %finclude%
copy %fpfilename%.f90 %finclude%

REM Delete .o files
del *.o
