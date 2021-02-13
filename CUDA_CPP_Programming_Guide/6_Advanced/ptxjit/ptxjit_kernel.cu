/*
 * Simple kernel for ptxjit demonstration.
 *
 */ 
 extern "C" __global__ void myKernel(int *data)
 {
 	int tid = blockIdx.x * blockDim.x + threadIdx.x;
	data[tid] = tid;
 }
