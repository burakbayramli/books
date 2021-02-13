/*
 * Copyright 1993-2015 NVIDIA Corporation.  All rights reserved.
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


#include "common.h"

///////////////////////////////////////////////////////////////////////////////
/// \brief warp image with a given displacement field, CUDA kernel.
/// \param[in]  width   image width
/// \param[in]  height  image height
/// \param[in]  stride  image stride
/// \param[in]  u       horizontal displacement
/// \param[in]  v       vertical displacement
/// \param[out] out     result
///////////////////////////////////////////////////////////////////////////////
__global__ void WarpingKernel(int width, int height, int stride,
                              const float *u, const float *v, float *out, cudaTextureObject_t texToWarp)
{
    const int ix = threadIdx.x + blockIdx.x * blockDim.x;
    const int iy = threadIdx.y + blockIdx.y * blockDim.y;

    const int pos = ix + iy * stride;

    if (ix >= width || iy >= height) return;

    float x = ((float)ix + u[pos] + 0.5f) / (float)width;
    float y = ((float)iy + v[pos] + 0.5f) / (float)height;

    out[pos] = tex2D<float>(texToWarp, x, y);
}

///////////////////////////////////////////////////////////////////////////////
/// \brief warp image with provided vector field, CUDA kernel wrapper.
///
/// For each output pixel there is a vector which tells which pixel
/// from a source image should be mapped to this particular output
/// pixel.
/// It is assumed that images and the vector field have the same stride and
/// resolution.
/// \param[in]  src source image
/// \param[in]  w   width
/// \param[in]  h   height
/// \param[in]  s   stride
/// \param[in]  u   horizontal displacement
/// \param[in]  v   vertical displacement
/// \param[out] out warped image
///////////////////////////////////////////////////////////////////////////////
static
void WarpImage(const float *src, int w, int h, int s,
               const float *u, const float *v, float *out)
{
    dim3 threads(32, 6);
    dim3 blocks(iDivUp(w, threads.x), iDivUp(h, threads.y));

    cudaTextureObject_t texToWarp;
    cudaResourceDesc            texRes;
    memset(&texRes,0,sizeof(cudaResourceDesc));

    texRes.resType            = cudaResourceTypePitch2D;
    texRes.res.pitch2D.devPtr = (void*)src;
    texRes.res.pitch2D.desc = cudaCreateChannelDesc<float>();
    texRes.res.pitch2D.width = w;
    texRes.res.pitch2D.height = h;
    texRes.res.pitch2D.pitchInBytes = s * sizeof(float);

    cudaTextureDesc             texDescr;
    memset(&texDescr,0,sizeof(cudaTextureDesc));

    texDescr.normalizedCoords = true;
    texDescr.filterMode       = cudaFilterModeLinear;
    texDescr.addressMode[0]   = cudaAddressModeMirror;
    texDescr.addressMode[1]   = cudaAddressModeMirror;
    texDescr.readMode = cudaReadModeElementType;

    checkCudaErrors(cudaCreateTextureObject(&texToWarp, &texRes, &texDescr, NULL));

    WarpingKernel<<<blocks, threads>>>(w, h, s, u, v, out, texToWarp);
}
