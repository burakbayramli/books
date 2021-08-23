Sample: matrixMul
Minimum spec: SM 3.0

This sample implements matrix multiplication which makes use of shared memory to ensure data reuse, the matrix multiplication is done using tiling approach. It has been written for clarity of exposition to illustrate various CUDA programming principles, not with the goal of providing the most performant generic kernel for matrix multiplication.

Key concepts:
CUDA Runtime API
Linear Algebra
