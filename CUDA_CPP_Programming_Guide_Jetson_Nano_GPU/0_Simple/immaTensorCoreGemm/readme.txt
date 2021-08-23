Sample: immaTensorCoreGemm
Minimum spec: SM 7.2

CUDA sample demonstrating a integer GEMM computation using the Warp
Matrix Multiply and Accumulate (WMMA) API for integer introduced in
CUDA 10. This sample demonstrates the use of the CUDA WMMA API
employing the Tensor Cores introduced in the Volta chip family for
faster matrix operations. In addition to that, it demonstrates the use
of the new CUDA function attribute
cudaFuncAttributeMaxDynamicSharedMemorySize that allows the
application to reserve an extended amount of shared memory than it is
available by default.

Key concepts:
Matrix Multiply
WMMA
Tensor Cores
