/*
 * Copyright 1993-2019 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

/* Vector addition: C = A + B.
 *
 * This sample is a very basic sample that implements element by element
 * vector addition. It loads a cuda fatbinary and runs vector addition kernel.
 * Uses both Driver and Runtime CUDA APIs for different purposes.
 */

// Includes
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <cstring>
#include <cuda_runtime.h>
#include <cuda.h>

// includes, project
#include <helper_cuda.h>
#include <helper_functions.h>

// includes, CUDA
#include <builtin_types.h>

using namespace std;

#ifndef FATBIN_FILE
#define FATBIN_FILE "vectorAdd_kernel64.fatbin"
#endif

// Variables
float *h_A;
float *h_B;
float *h_C;
float *d_A;
float *d_B;
float *d_C;

// Functions
int CleanupNoFailure(CUcontext &cuContext);
void RandomInit(float *, int);
bool findModulePath(const char *, string &, char **, ostringstream &);

static void check(CUresult result, char const *const func, const char *const file,
           int const line) {
  if (result) {
    fprintf(stderr, "CUDA error at %s:%d code=%d \"%s\" \n", file, line,
            static_cast<unsigned int>(result), func);
    exit(EXIT_FAILURE);
  }
}

#define checkCudaDrvErrors(val) check((val), #val, __FILE__, __LINE__)

// Host code
int main(int argc, char **argv)
{
    printf("simpleDrvRuntime..\n");
    int N = 50000, devID = 0;
    size_t  size = N * sizeof(float);
    CUdevice cuDevice;
    CUfunction vecAdd_kernel;
    CUmodule cuModule = 0;
    CUcontext cuContext;

    // Initialize
    checkCudaDrvErrors(cuInit(0));

    cuDevice = findCudaDevice(argc, (const char **)argv);
    // Create context
    checkCudaDrvErrors(cuCtxCreate(&cuContext, 0, cuDevice));

    // first search for the module path before we load the results
    string module_path;
    ostringstream fatbin;

    if (!findModulePath(FATBIN_FILE, module_path, argv, fatbin))
    {
        exit(EXIT_FAILURE);
    }
    else
    {
        printf("> initCUDA loading module: <%s>\n", module_path.c_str());
    }

    if (!fatbin.str().size())
    {
        printf("fatbin file empty. exiting..\n");
        exit(EXIT_FAILURE);
    }

    // Create module from binary file (FATBIN)
    checkCudaDrvErrors(cuModuleLoadData(&cuModule, fatbin.str().c_str()));

    // Get function handle from module
    checkCudaDrvErrors(cuModuleGetFunction(&vecAdd_kernel, cuModule, "VecAdd_kernel"));

    // Allocate input vectors h_A and h_B in host memory
    h_A = (float *)malloc(size);
    h_B = (float *)malloc(size);
    h_C = (float *)malloc(size);

    // Initialize input vectors
    RandomInit(h_A, N);
    RandomInit(h_B, N);

    // Allocate vectors in device memory
    checkCudaErrors(cudaMalloc((void**)(&d_A), size));
    checkCudaErrors(cudaMalloc((void**)(&d_B), size));
    checkCudaErrors(cudaMalloc((void**)(&d_C), size));

    cudaStream_t stream;
    checkCudaErrors(cudaStreamCreateWithFlags(&stream, cudaStreamNonBlocking));
    // Copy vectors from host memory to device memory
    checkCudaErrors(cudaMemcpyAsync(d_A, h_A, size, cudaMemcpyHostToDevice, stream));
    checkCudaErrors(cudaMemcpyAsync(d_B, h_B, size, cudaMemcpyHostToDevice, stream));

    int threadsPerBlock = 256;
    int blocksPerGrid   = (N + threadsPerBlock - 1) / threadsPerBlock;

    void *args[] = { &d_A, &d_B, &d_C, &N };

    // Launch the CUDA kernel
    checkCudaDrvErrors(cuLaunchKernel(vecAdd_kernel,  blocksPerGrid, 1, 1,
                           threadsPerBlock, 1, 1,
                           0,
                           stream, args, NULL));

    // Copy result from device memory to host memory
    // h_C contains the result in host memory
    checkCudaErrors(cudaMemcpyAsync(h_C, d_C, size, cudaMemcpyDeviceToHost, stream));
    checkCudaErrors(cudaStreamSynchronize(stream));
    // Verify result
    int i;

    for (i = 0; i < N; ++i)
    {
        float sum = h_A[i] + h_B[i];

        if (fabs(h_C[i] - sum) > 1e-7f)
        {
            break;
        }
    }

    checkCudaDrvErrors(cuModuleUnload(cuModule));
    CleanupNoFailure(cuContext);
    printf("%s\n", (i==N) ? "Result = PASS" : "Result = FAIL");

    exit((i==N) ? EXIT_SUCCESS : EXIT_FAILURE);
}

int CleanupNoFailure(CUcontext &cuContext)
{
    // Free device memory
    checkCudaErrors(cudaFree(d_A));
    checkCudaErrors(cudaFree(d_B));
    checkCudaErrors(cudaFree(d_C));

    // Free host memory
    if (h_A)
    {
        free(h_A);
    }

    if (h_B)
    {
        free(h_B);
    }

    if (h_C)
    {
        free(h_C);
    }

    checkCudaDrvErrors(cuCtxDestroy(cuContext));

    return EXIT_SUCCESS;
}
// Allocates an array with random float entries.
void RandomInit(float *data, int n)
{
    for (int i = 0; i < n; ++i)
    {
        data[i] = rand() / (float)RAND_MAX;
    }
}

bool inline
findModulePath(const char *module_file, string &module_path, char **argv, ostringstream &ostrm)
{
    char *actual_path = sdkFindFilePath(module_file, argv[0]);

    if (actual_path)
    {
        module_path = actual_path;
    }
    else
    {
        printf("> findModulePath file not found: <%s> \n", module_file);
        return false;
    }

    if (module_path.empty())
    {
        printf("> findModulePath could not find file: <%s> \n", module_file);
        return false;
    }
    else
    {
        printf("> findModulePath found file at <%s>\n", module_path.c_str());
        if (module_path.rfind("fatbin") != string::npos)
        {
            ifstream fileIn(module_path.c_str(), ios::binary);
            ostrm << fileIn.rdbuf();
        }
        return true;
    }
}
