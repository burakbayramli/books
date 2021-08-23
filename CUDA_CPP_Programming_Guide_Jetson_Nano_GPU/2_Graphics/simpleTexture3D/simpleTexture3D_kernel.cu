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

#ifndef _SIMPLETEXTURE3D_KERNEL_CU_
#define _SIMPLETEXTURE3D_KERNEL_CU_


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <helper_cuda.h>
#include <helper_math.h>

typedef unsigned int  uint;
typedef unsigned char uchar;


cudaArray *d_volumeArray = 0;
cudaTextureObject_t tex;    // 3D texture

__global__ void
d_render(uint *d_output, uint imageW, uint imageH, float w, cudaTextureObject_t	texObj)
{
    uint x = __umul24(blockIdx.x, blockDim.x) + threadIdx.x;
    uint y = __umul24(blockIdx.y, blockDim.y) + threadIdx.y;

    float u = x / (float) imageW;
    float v = y / (float) imageH;
    // read from 3D texture
    float voxel = tex3D<float>(texObj, u, v, w);

    if ((x < imageW) && (y < imageH))
    {
        // write output color
        uint i = __umul24(y, imageW) + x;
        d_output[i] = voxel*255;
    }
}

extern "C"
void setTextureFilterMode(bool bLinearFilter)
{
    if (tex)
    {
        checkCudaErrors(cudaDestroyTextureObject(tex));
    }
    cudaResourceDesc            texRes;
    memset(&texRes,0,sizeof(cudaResourceDesc));

    texRes.resType            = cudaResourceTypeArray;
    texRes.res.array.array    = d_volumeArray;

    cudaTextureDesc             texDescr;
    memset(&texDescr,0,sizeof(cudaTextureDesc));

    texDescr.normalizedCoords = true;
    texDescr.filterMode       = bLinearFilter ? cudaFilterModeLinear : cudaFilterModePoint;;
    texDescr.addressMode[0] = cudaAddressModeWrap;
    texDescr.addressMode[1] = cudaAddressModeWrap;
    texDescr.addressMode[2] = cudaAddressModeWrap;
    texDescr.readMode = cudaReadModeNormalizedFloat;

    checkCudaErrors(cudaCreateTextureObject(&tex, &texRes, &texDescr, NULL));
}


extern "C"
void initCuda(const uchar *h_volume, cudaExtent volumeSize)
{
    // create 3D array
    cudaChannelFormatDesc channelDesc = cudaCreateChannelDesc<uchar>();
    checkCudaErrors(cudaMalloc3DArray(&d_volumeArray, &channelDesc, volumeSize));

    // copy data to 3D array
    cudaMemcpy3DParms copyParams = {0};
    copyParams.srcPtr   = make_cudaPitchedPtr((void *)h_volume, volumeSize.width*sizeof(uchar), volumeSize.width, volumeSize.height);
    copyParams.dstArray = d_volumeArray;
    copyParams.extent   = volumeSize;
    copyParams.kind     = cudaMemcpyHostToDevice;
    checkCudaErrors(cudaMemcpy3D(&copyParams));

    cudaResourceDesc            texRes;
    memset(&texRes,0,sizeof(cudaResourceDesc));

    texRes.resType            = cudaResourceTypeArray;
    texRes.res.array.array    = d_volumeArray;

    cudaTextureDesc             texDescr;
    memset(&texDescr,0,sizeof(cudaTextureDesc));

    texDescr.normalizedCoords = true; // access with normalized texture coordinates
    texDescr.filterMode       = cudaFilterModeLinear; // linear interpolation
    // wrap texture coordinates
    texDescr.addressMode[0] = cudaAddressModeWrap;
    texDescr.addressMode[1] = cudaAddressModeWrap;
    texDescr.addressMode[2] = cudaAddressModeWrap;
    texDescr.readMode = cudaReadModeNormalizedFloat;

    checkCudaErrors(cudaCreateTextureObject(&tex, &texRes, &texDescr, NULL));
}

extern "C"
void render_kernel(dim3 gridSize, dim3 blockSize, uint *d_output, uint imageW, uint imageH, float w)
{

    d_render<<<gridSize, blockSize>>>(d_output, imageW, imageH, w, tex);
}

void cleanupCuda()
{
     if (tex)
    {
        checkCudaErrors(cudaDestroyTextureObject(tex));
    }   
    if (d_volumeArray)
    {
        checkCudaErrors(cudaFreeArray(d_volumeArray));
    }
}

#endif // #ifndef _SIMPLETEXTURE3D_KERNEL_CU_
