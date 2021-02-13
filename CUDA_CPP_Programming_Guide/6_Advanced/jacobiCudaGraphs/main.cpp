/*
 * Copyright 1993-2019 NVIDIA Corporation.  All rights reserved.
 *
 * NVIDIA Corporation and its licensors retain all intellectual property and
 * proprietary rights in and to this software and related documentation.
 * Any use, reproduction, disclosure, or distribution of this software
 * and related documentation without an express license agreement from
 * NVIDIA Corporation is strictly prohibited.
 *
 * Please refer to the applicable NVIDIA end user license agreement (EULA)
 * associated with this source code for terms and conditions that govern
 * your use of this NVIDIA software.
 *
 */

// This sample demonstrates Instantiated CUDA Graph Update
// with Jacobi Iterative Method in 3 different methods:
// 1 - JacobiMethodGpuCudaGraphExecKernelSetParams() - CUDA Graph with cudaGraphExecKernelNodeSetParams()
// 2 - JacobiMethodGpuCudaGraphExecUpdate() - CUDA Graph with cudaGraphExecUpdate()
// 3 - JacobiMethodGpu() - Non CUDA Graph method

// Jacobi method on a linear system A*x = b,
// where A is diagonally dominant and the exact solution consists
// of all ones.  
// The dimension N_ROWS is included in jacobi.h

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <cuda_runtime.h>
#include <helper_cuda.h>
#include <helper_timer.h>
#include "jacobi.h"

// Run the Jacobi method for A*x = b on GPU with CUDA Graph - cudaGraphExecKernelNodeSetParams().
extern double JacobiMethodGpuCudaGraphExecKernelSetParams(const float *A, const double *b, 
                          const float conv_threshold,
                          const int max_iter, double *x,
                          double *x_new, cudaStream_t stream);

// Run the Jacobi method for A*x = b on GPU with Instantiated CUDA Graph Update API - cudaGraphExecUpdate().
extern double JacobiMethodGpuCudaGraphExecUpdate(const float *A, const double *b, 
                          const float conv_threshold,
                          const int max_iter, double *x,
                          double *x_new, cudaStream_t stream);

// Run the Jacobi method for A*x = b on GPU without CUDA Graph.
extern double JacobiMethodGpu(const float *A, const double *b, 
                          const float conv_threshold,
                          const int max_iter, double *x,
                          double *x_new, cudaStream_t stream);


// creates N_ROWS x N_ROWS matrix A with N_ROWS+1 on the diagonal and 1 elsewhere.
// The elements of the right hand side b
// all equal 2*n, hence the exact solution x
// to A*x = b is a vector of ones.
void createLinearSystem(float *A, double *b);

// Run the Jacobi method for A*x = b on CPU.
void JacobiMethodCPU(float *A, double *b, float conv_threshold, int max_iter,
                      int *numit, double *x);


int main (int argc, char **argv)
{
    if (checkCmdLineFlag(argc, (const char **)argv, "help"))
    {
        printf("Command line: jacobiCudaGraphs [-option]\n");
        printf("Valid options:\n");
        printf("-gpumethod=<0,1 or 2>  : 0 - [Default] JacobiMethodGpuCudaGraphExecKernelSetParams\n");
        printf("                       : 1 - JacobiMethodGpuCudaGraphExecUpdate\n");
        printf("                       : 2 - JacobiMethodGpu - Non CUDA Graph\n");
        printf("-device=device_num     : cuda device id");
        printf("-help         : Output a help message\n");
        exit(EXIT_SUCCESS);
    }

    int gpumethod = 0;
    if (checkCmdLineFlag(argc, (const char **)argv, "gpumethod"))
    {
        gpumethod = getCmdLineArgumentInt(argc, (const char **)argv, "gpumethod");

        if (gpumethod < 0 || gpumethod > 2)
        {
            printf("Error: gpumethod must be 0 or 1 or 2, gpumethod=%d is invalid\n", gpumethod);
            exit(EXIT_SUCCESS);
        }
    }

    int dev = findCudaDevice(argc, (const char**)argv);

    double *b = NULL;
    float *A = NULL;
    b = (double*) calloc(N_ROWS, sizeof(double));
    A = (float*) calloc(N_ROWS*N_ROWS,sizeof(float));

    createLinearSystem(A,b);
    double *x = NULL;
    // start with array of all zeroes
    x = (double*) calloc(N_ROWS,sizeof(double));

    float conv_threshold = 1.0e-2;
    int max_iter = 4*N_ROWS*N_ROWS;
    int cnt = 0;

    // create timer
    StopWatchInterface *timerCPU = NULL, *timerGpu = NULL;
    sdkCreateTimer(&timerCPU);

    sdkStartTimer(&timerCPU);
    JacobiMethodCPU(A, b, conv_threshold, max_iter, &cnt, x);

    double sum = 0.0;
    // Compute error
    for(int i=0; i<N_ROWS; i++)
    {
       double d = x[i] - 1.0;
       sum += fabs(d);
    }
    sdkStopTimer(&timerCPU);
    printf("CPU iterations : %d\n",cnt);
    printf("CPU error : %.3e\n", sum);
    printf("CPU Processing time: %f (ms)\n", sdkGetTimerValue(&timerCPU));

    float  *d_A;
    double *d_b,*d_x, *d_x_new;
    cudaStream_t stream1;
    checkCudaErrors(cudaStreamCreateWithFlags(&stream1, cudaStreamNonBlocking));
    checkCudaErrors(cudaMalloc(&d_b, sizeof(double)*N_ROWS));
    checkCudaErrors(cudaMalloc(&d_A, sizeof(float)*N_ROWS*N_ROWS));
    checkCudaErrors(cudaMalloc(&d_x, sizeof(double)*N_ROWS));
    checkCudaErrors(cudaMalloc(&d_x_new, sizeof(double)*N_ROWS));

    checkCudaErrors(cudaMemsetAsync(d_x, 0, sizeof(double)*N_ROWS, stream1));
    checkCudaErrors(cudaMemsetAsync(d_x_new, 0, sizeof(double)*N_ROWS, stream1));
    checkCudaErrors(cudaMemcpyAsync(d_A, A, sizeof(float)*N_ROWS*N_ROWS, cudaMemcpyHostToDevice, stream1));
    checkCudaErrors(cudaMemcpyAsync(d_b, b, sizeof(double)*N_ROWS, cudaMemcpyHostToDevice, stream1));

    sdkCreateTimer(&timerGpu);
    sdkStartTimer(&timerGpu);

    double sumGPU = 0.0;
    if (gpumethod == 0)
    {
        sumGPU = JacobiMethodGpuCudaGraphExecKernelSetParams(d_A, d_b, conv_threshold, max_iter, d_x, d_x_new, stream1);
    }
    else if (gpumethod == 1)
    {
        sumGPU = JacobiMethodGpuCudaGraphExecUpdate(d_A, d_b, conv_threshold, max_iter, d_x, d_x_new, stream1);
    }
    else if (gpumethod == 2)
    {
        sumGPU = JacobiMethodGpu(d_A, d_b, conv_threshold, max_iter, d_x, d_x_new, stream1);
    }

    sdkStopTimer(&timerGpu);
    printf("GPU Processing time: %f (ms)\n", sdkGetTimerValue(&timerGpu));

    checkCudaErrors(cudaFree(d_b));
    checkCudaErrors(cudaFree(d_A));
    checkCudaErrors(cudaFree(d_x));
    checkCudaErrors(cudaFree(d_x_new));

    printf("&&&& jacobiCudaGraphs %s\n", (fabs(sum-sumGPU) < conv_threshold) ? "PASSED" : "FAILED");

    return (fabs(sum-sumGPU) < conv_threshold) ? EXIT_SUCCESS : EXIT_FAILURE;
}

void createLinearSystem(float *A, double *b)
{
   int i,j;
   for(i=0; i<N_ROWS; i++)
   {
      b[i] = 2.0*N_ROWS;
      for(j=0; j<N_ROWS; j++) A[i*N_ROWS + j] = 1.0;
      A[i*N_ROWS + i] = N_ROWS + 1.0;
   }
}

void JacobiMethodCPU(float *A, double *b, float conv_threshold, int max_iter,
                    int *num_iter, double *x)
{
    double *x_new;
    x_new = (double*) calloc(N_ROWS,sizeof(double));
    int k;

    for(k=0; k<max_iter; k++)
    {
        double sum = 0.0;
        for(int i=0; i<N_ROWS; i++)
        {
           double temp_dx = b[i];
           for(int j=0; j<N_ROWS; j++)
              temp_dx -= A[i*N_ROWS + j]*x[j]; 
           temp_dx /= A[i*N_ROWS + i];
           x_new[i] += temp_dx;
           sum += fabs(temp_dx);
        } 

        for(int i=0; i<N_ROWS; i++) x[i] = x_new[i];

        if(sum <= conv_threshold) break;
    }
    *num_iter = k+1;
    free(x_new);
}
