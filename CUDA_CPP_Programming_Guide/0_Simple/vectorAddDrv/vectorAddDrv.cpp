/*
 * Copyright 1993-2015 NVIDIA Corporation.  All rights reserved.
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
 * vector addition. It is the same as the sample illustrating Chapter 3
 * of the programming guide with some additions like error checking.
 *
 */

// Includes
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <cstring>
#include <cuda.h>

// includes, project
#include <helper_cuda_drvapi.h>
#include <helper_functions.h>

// includes, CUDA
#include <builtin_types.h>

using namespace std;

// Variables
CUdevice cuDevice;
CUcontext cuContext;
CUmodule cuModule;
CUfunction vecAdd_kernel;
float *h_A;
float *h_B;
float *h_C;
CUdeviceptr d_A;
CUdeviceptr d_B;
CUdeviceptr d_C;

// Functions
int CleanupNoFailure();
void RandomInit(float *, int);
bool findModulePath(const char *, string &, char **, string &);

//define input ptx file for different platforms
#if defined(_WIN64) || defined(__LP64__)
#define PTX_FILE "vectorAdd_kernel64.ptx"
#else
#define PTX_FILE "vectorAdd_kernel32.ptx"
#endif

// Host code
int main(int argc, char **argv)
{
    printf("Vector Addition (Driver API)\n");
    int N = 50000, devID = 0;
    size_t  size = N * sizeof(float);

    // Initialize
    checkCudaErrors(cuInit(0));

    cuDevice = findCudaDeviceDRV(argc, (const char **)argv);
    // Create context
    checkCudaErrors(cuCtxCreate(&cuContext, 0, cuDevice));

    // first search for the module path before we load the results
    string module_path, ptx_source;

    if (!findModulePath(PTX_FILE, module_path, argv, ptx_source))
    {
        if (!findModulePath("vectorAdd_kernel.cubin", module_path, argv, ptx_source))
        {
            printf("> findModulePath could not find <vectorAdd> ptx or cubin\n");
            exit(EXIT_FAILURE);
        }
    }
    else
    {
        printf("> initCUDA loading module: <%s>\n", module_path.c_str());
    }

    // Create module from binary file (PTX or CUBIN)
    if (module_path.rfind("ptx") != string::npos)
    {
        // in this branch we use compilation with parameters
        const unsigned int jitNumOptions = 3;
        CUjit_option *jitOptions = new CUjit_option[jitNumOptions];
        void **jitOptVals = new void *[jitNumOptions];

        // set up size of compilation log buffer
        jitOptions[0] = CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES;
        int jitLogBufferSize = 1024;
        jitOptVals[0] = (void *)(size_t)jitLogBufferSize;

        // set up pointer to the compilation log buffer
        jitOptions[1] = CU_JIT_INFO_LOG_BUFFER;
        char *jitLogBuffer = new char[jitLogBufferSize];
        jitOptVals[1] = jitLogBuffer;

        // set up pointer to set the Maximum # of registers for a particular kernel
        jitOptions[2] = CU_JIT_MAX_REGISTERS;
        int jitRegCount = 32;
        jitOptVals[2] = (void *)(size_t)jitRegCount;

        checkCudaErrors(cuModuleLoadDataEx(&cuModule, ptx_source.c_str(), jitNumOptions, jitOptions, (void **)jitOptVals));

        printf("> PTX JIT log:\n%s\n", jitLogBuffer);
    }
    else
    {
        checkCudaErrors(cuModuleLoad(&cuModule, module_path.c_str()));
    }

    // Get function handle from module
    checkCudaErrors(cuModuleGetFunction(&vecAdd_kernel, cuModule, "VecAdd_kernel"));

    // Allocate input vectors h_A and h_B in host memory
    h_A = (float *)malloc(size);
    h_B = (float *)malloc(size);
    h_C = (float *)malloc(size);


    // Initialize input vectors
    RandomInit(h_A, N);
    RandomInit(h_B, N);

    // Allocate vectors in device memory
    checkCudaErrors(cuMemAlloc(&d_A, size));

    checkCudaErrors(cuMemAlloc(&d_B, size));

    checkCudaErrors(cuMemAlloc(&d_C, size));

    // Copy vectors from host memory to device memory
    checkCudaErrors(cuMemcpyHtoD(d_A, h_A, size));

    checkCudaErrors(cuMemcpyHtoD(d_B, h_B, size));

    if (1)
    {
        // This is the new CUDA 4.0 API for Kernel Parameter Passing and Kernel Launch (simpler method)

        // Grid/Block configuration
        int threadsPerBlock = 256;
        int blocksPerGrid   = (N + threadsPerBlock - 1) / threadsPerBlock;

        void *args[] = { &d_A, &d_B, &d_C, &N };

        // Launch the CUDA kernel
        checkCudaErrors(cuLaunchKernel(vecAdd_kernel,  blocksPerGrid, 1, 1,
                               threadsPerBlock, 1, 1,
                               0,
                               NULL, args, NULL));
    }
    else
    {
        // This is the new CUDA 4.0 API for Kernel Parameter Passing and Kernel Launch (advanced method)
        int offset = 0;
        void *argBuffer[16];
        *((CUdeviceptr *)&argBuffer[offset]) = d_A;
        offset += sizeof(d_A);
        *((CUdeviceptr *)&argBuffer[offset]) = d_B;
        offset += sizeof(d_B);
        *((CUdeviceptr *)&argBuffer[offset]) = d_C;
        offset += sizeof(d_C);
        *((int *)&argBuffer[offset]) = N;
        offset += sizeof(N);

        // Grid/Block configuration
        int threadsPerBlock = 256;
        int blocksPerGrid   = (N + threadsPerBlock - 1) / threadsPerBlock;

        // Launch the CUDA kernel
        checkCudaErrors(cuLaunchKernel(vecAdd_kernel,  blocksPerGrid, 1, 1,
                               threadsPerBlock, 1, 1,
                               0,
                               NULL, NULL, argBuffer));

    }

#ifdef _DEBUG
    checkCudaErrors(cuCtxSynchronize());
#endif

    // Copy result from device memory to host memory
    // h_C contains the result in host memory
    checkCudaErrors(cuMemcpyDtoH(h_C, d_C, size));

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

    CleanupNoFailure();
    printf("%s\n", (i==N) ? "Result = PASS" : "Result = FAIL");

    exit((i==N) ? EXIT_SUCCESS : EXIT_FAILURE);
}

int CleanupNoFailure()
{
    // Free device memory
    checkCudaErrors(cuMemFree(d_A));
    checkCudaErrors(cuMemFree(d_B));
    checkCudaErrors(cuMemFree(d_C));

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

    checkCudaErrors(cuCtxDestroy(cuContext));

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
findModulePath(const char *module_file, string &module_path, char **argv, string &ptx_source)
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

        if (module_path.rfind(".ptx") != string::npos)
        {
            FILE *fp = fopen(module_path.c_str(), "rb");
            fseek(fp, 0, SEEK_END);
            int file_size = ftell(fp);
            char *buf = new char[file_size+1];
            fseek(fp, 0, SEEK_SET);
            fread(buf, sizeof(char), file_size, fp);
            fclose(fp);
            buf[file_size] = '\0';
            ptx_source = buf;
            delete[] buf;
        }

        return true;
    }
}
