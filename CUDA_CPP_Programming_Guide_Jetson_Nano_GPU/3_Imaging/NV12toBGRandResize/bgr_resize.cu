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

// Implements BGR 3 progressive planars frames batch resize

#include <cuda.h>
#include <cuda_runtime.h>
#include "resize_convert.h"

__global__ void resizeBGRplanarBatchKernel(cudaTextureObject_t texSrc,
    float *pDst, int nDstPitch, int nDstHeight, int nSrcHeight,
    int batch, float scaleX, float scaleY,
    int cropX, int cropY, int cropW, int cropH) {
    int x = threadIdx.x + blockIdx.x * blockDim.x;
    int y = threadIdx.y + blockIdx.y * blockDim.y;

    if (x >= (int)(cropW/scaleX) || y >= (int)(cropH/scaleY))
        return;

    int frameSize = nDstPitch*nDstHeight;
    float *p = NULL;
    for (int i = blockIdx.z; i < batch; i += gridDim.z) {
        #pragma unroll
        for (int channel=0; channel < 3; channel++){
            p = pDst + i * 3 * frameSize + y * nDstPitch + x + channel * frameSize;
            *p = tex2D<float>(texSrc, x * scaleX + cropX,
                                ((3 * i + channel) * nSrcHeight + y * scaleY + cropY));
        }
    }
}


static void resizeBGRplanarBatchCore(
        float *dpSrc, int nSrcPitch, int nSrcWidth, int nSrcHeight,
        float *dpDst, int nDstPitch, int nDstWidth, int nDstHeight,
        int nBatchSize, cudaStream_t stream, bool whSameResizeRatio,
        int cropX, int cropY, int cropW, int cropH) {
    cudaTextureObject_t texSrc[2];
    int nTiles = 1, h, iTile;

    h = nSrcHeight * 3 * nBatchSize;
    while ((h + nTiles - 1) / nTiles > 65536)
        nTiles++;

    if (nTiles > 2)
        return;

    int batchTile = nBatchSize / nTiles;
    int batchTileLast = nBatchSize - batchTile * (nTiles-1);

    for (iTile = 0; iTile < nTiles; ++iTile) {
        int bs = (iTile == nTiles - 1) ? batchTileLast : batchTile;
        float *dpSrcNew = dpSrc +
            iTile * (batchTile * 3 * nSrcHeight * nSrcPitch);

        cudaResourceDesc resDesc = {};
        resDesc.resType = cudaResourceTypePitch2D;
        resDesc.res.pitch2D.devPtr = dpSrcNew;
        resDesc.res.pitch2D.desc = cudaCreateChannelDesc<float>();
        resDesc.res.pitch2D.width = nSrcWidth;
        resDesc.res.pitch2D.height = bs * 3 * nSrcHeight;
        resDesc.res.pitch2D.pitchInBytes = nSrcPitch * sizeof(float);
        cudaTextureDesc texDesc = {};
        texDesc.filterMode = cudaFilterModeLinear;
        texDesc.readMode = cudaReadModeElementType;

        cudaCreateTextureObject(&texSrc[iTile], &resDesc, &texDesc, NULL);
        float *dpDstNew = dpDst +
            iTile * (batchTile * 3 * nDstHeight * nDstPitch);

        if(cropW == 0 || cropH == 0) {
            cropX = 0;
            cropY = 0;
            cropW = nSrcWidth;
            cropH = nSrcHeight;
        }

        float scaleX = (cropW*1.0f / nDstWidth);
        float scaleY = (cropH*1.0f / nDstHeight);

        if(whSameResizeRatio == true)
            scaleX = scaleY = scaleX > scaleY ? scaleX : scaleY;
        dim3 block(32, 32, 1);

        size_t blockDimZ = bs;
        // Restricting blocks in Z-dim till 32 to not launch too many blocks
        blockDimZ = (blockDimZ > 32) ? 32 : blockDimZ;
        dim3 grid((cropW*1.0f/scaleX + block.x - 1) / block.x,
                  (cropH*1.0f/scaleY + block.y - 1) / block.y, blockDimZ);

        resizeBGRplanarBatchKernel<<<grid, block, 0, stream>>>
                (texSrc[iTile], dpDstNew, nDstPitch, nDstHeight, nSrcHeight,
                bs, scaleX, scaleY, cropX, cropY, cropW, cropH);

    }

    for (iTile = 0; iTile < nTiles; ++iTile)
        cudaDestroyTextureObject(texSrc[iTile]);
}

void resizeBGRplanarBatch(
        float *dpSrc, int nSrcPitch, int nSrcWidth, int nSrcHeight,
        float *dpDst, int nDstPitch, int nDstWidth, int nDstHeight,
        int nBatchSize, cudaStream_t stream,
        int cropX, int cropY, int cropW, int cropH, bool whSameResizeRatio) {
    resizeBGRplanarBatchCore(dpSrc, nSrcPitch, nSrcWidth, nSrcHeight,
        dpDst, nDstPitch, nDstWidth, nDstHeight, nBatchSize, stream,
        whSameResizeRatio, cropX, cropY, cropW, cropH);
}
