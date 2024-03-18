# this MAKEFILE is intended to compile the example code for LINUX/UBUNTU
#
# g++ options:
#	-I dir : adds the directory dir to the list of directories to be searched for header files during preprocessing
#	-c : compile or assemble, but do not link
#	-fPIC : emit position-independent code, suitable for dynamic linking and avoiding any limit on the size of the global offset table
#	-shared : shared library

CXX = g++

# LINK
c_vlm.so: c_vlm.o c_lattice.o c_downwash.o c_boundary.o c_results.o
	g++ -shared -fPIC c_vlm.o c_lattice.o c_downwash.o c_boundary.o c_results.o -o c_vlm.so

# COMPILE
c_vlm.o: c_vlm.cpp c_lattice.cpp c_downwash.cpp c_boundary.cpp c_results.cpp c_vlm.h
	g++ -c -fPIC c_vlm.cpp c_lattice.cpp c_downwash.cpp c_boundary.cpp c_results.cpp -I /usr/include/python3.6 -I /usr/lib/python3/dist-packages/numpy/core/include/

