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

// Implements interlace NV12 frames batch resize

#include <cuda.h>
#include <cuda_runtime.h>
#include "resize_convert.h"

__global__ static void resizeNV12BatchKernel(cudaTextureObject_t texSrcLuma,
                                             cudaTextureObject_t texSrcChroma,
                                             uint8_t *pDstNv12, int nSrcWidth,
                                             int nSrcHeight, int nDstPitch,
                                             int nDstWidth, int nDstHeight,
                                             int nBatchSize) {
  int x = threadIdx.x + blockIdx.x * blockDim.x;
  int y = threadIdx.y + blockIdx.y * blockDim.y;

  int px = x * 2, py = y * 2;

  if ((px + 1) >= nDstWidth || (py + 1) >= nDstHeight) return;

  float fxScale = 1.0f * nSrcWidth / nDstWidth;
  float fyScale = 1.0f * nSrcHeight / nDstHeight;

  uint8_t *p = pDstNv12 + px + py * nDstPitch;
  int hh = nDstHeight * 3 / 2;
  int nByte = nDstPitch * hh;
  int px_fxScale = px * fxScale;
  int px_fxScale_1 = (px + 1) * fxScale;
  int py_fyScale = py * fyScale;
  int py_fyScale_1 = (py + 1) * fyScale;

  for (int i = blockIdx.z; i < nBatchSize; i+=gridDim.z) {
    *(uchar2 *)p = make_uchar2(tex2D<uint8_t>(texSrcLuma, px_fxScale, py_fyScale),
                          tex2D<uint8_t>(texSrcLuma, px_fxScale_1, py_fyScale));
    *(uchar2 *)(p + nDstPitch) =
        make_uchar2(tex2D<uint8_t>(texSrcLuma, px_fxScale, py_fyScale_1),
               tex2D<uint8_t>(texSrcLuma, px_fxScale_1, py_fyScale_1));
    *(uchar2 *)(p + (nDstHeight - y) * nDstPitch) = tex2D<uchar2>(
        texSrcChroma, x * fxScale, (hh * i + nDstHeight + y) * fyScale);
    p += nByte;
    py += hh;
  }
}

void resizeNV12Batch(uint8_t *dpSrc, int nSrcPitch, int nSrcWidth,
                     int nSrcHeight, uint8_t *dpDst, int nDstPitch,
                     int nDstWidth, int nDstHeight, int nBatchSize,
                     cudaStream_t stream) {
  int hhSrc = ceilf(nSrcHeight * 3.0f / 2.0f);
  cudaResourceDesc resDesc = {};
  resDesc.resType = cudaResourceTypePitch2D;
  resDesc.res.pitch2D.devPtr = dpSrc;
  resDesc.res.pitch2D.desc = cudaCreateChannelDesc<uint8_t>();
  resDesc.res.pitch2D.width = nSrcWidth;
  resDesc.res.pitch2D.height = hhSrc * nBatchSize;
  resDesc.res.pitch2D.pitchInBytes = nSrcPitch;

  cudaTextureDesc texDesc = {};
  texDesc.filterMode = cudaFilterModePoint;
  texDesc.readMode = cudaReadModeElementType;

  cudaTextureObject_t texLuma = 0;
  checkCudaErrors(cudaCreateTextureObject(&texLuma, &resDesc, &texDesc, NULL));

  resDesc.res.pitch2D.desc = cudaCreateChannelDesc<uchar2>();
  resDesc.res.pitch2D.width /= 2;

  cudaTextureObject_t texChroma = 0;
  checkCudaErrors(cudaCreateTextureObject(&texChroma, &resDesc, &texDesc, NULL));

  dim3 block(32, 32, 1);

  size_t blockDimZ = nBatchSize;

  // Restricting blocks in Z-dim till 32 to not launch too many blocks
  blockDimZ = (blockDimZ > 32) ? 32 : blockDimZ;

  dim3 grid((nDstWidth / 2 + block.x) / block.x,
            (nDstHeight / 2 + block.y) / block.y, blockDimZ);
  resizeNV12BatchKernel<<<grid, block, 0, stream>>>(
      texLuma, texChroma, dpDst, nSrcWidth, nSrcHeight, nDstPitch, nDstWidth,
      nDstHeight, nBatchSize);

  checkCudaErrors(cudaDestroyTextureObject(texLuma));
  checkCudaErrors(cudaDestroyTextureObject(texChroma));
}
