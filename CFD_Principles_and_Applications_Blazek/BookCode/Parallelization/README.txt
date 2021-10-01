This directory contains examples of parallel programming using Asynchronous
Agents Library, CUDA, MPI, and OpenMP (Section 9.6 of the book). The program
to be parallelized solves the 2-D Laplace equation on a structured,
rectangular grid using the Jacobi iteration. The boundary values are kept
fixed during the iterations.

The examples are written in C (CUDA and MPI) and in C++. In the case of MPI,
compilation is controlled by a Makefile. In the other cases, project files
for Visual Studio (VS 2012) are provided. Note that all examples, except for
OpenMP, require the presence of corresponding libraries and compilation tools
(see references in the book for download links). The OpenMP program does not
need any special provisions, any recent C++ compiler is sufficient.
