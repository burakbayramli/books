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

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fstream>
#include <iostream>

#include <cuda.h>
#include <cuda_runtime.h>

#include "resize_convert.h"
#include "utils.h"

__global__ void floatToChar(float *src, unsigned char *dst, int height,
                            int width, int batchSize) {
  int x = threadIdx.x + blockIdx.x * blockDim.x;

  if (x >= height * width) return;

  int offset = height * width * 3;

  for (int j = 0; j < batchSize; j++) {
    // b
    *(dst + j * offset + x * 3 + 0) =
        (unsigned char)*(src + j * offset + height * width * 0 + x);
    // g
    *(dst + j * offset + x * 3 + 1) =
        (unsigned char)*(src + j * offset + height * width * 1 + x);
    // r
    *(dst + j * offset + x * 3 + 2) =
        (unsigned char)*(src + j * offset + height * width * 2 + x);
  }
}

void floatPlanarToChar(float *src, unsigned char *dst, int height, int width,
                       int batchSize) {
  floatToChar<<<(height * width - 1) / 1024 + 1, 1024, 0, NULL>>>(
      src, dst, height, width, batchSize);
}

void dumpRawBGR(float *d_srcBGR, int pitch, int width, int height,
                int batchSize, char *folder, char *tag) {
  float *bgr, *d_bgr;
  int frameSize;
  char directory[120];
  char mkdir_cmd[256];
#if !defined(_WIN32)
  sprintf(directory, "output/%s", folder);
  sprintf(mkdir_cmd, "mkdir -p %s 2> /dev/null", directory);
#else
  sprintf(directory, "output\\%s", folder);
  sprintf(mkdir_cmd, "mkdir %s 2> nul", directory);
#endif

  int ret = system(mkdir_cmd);

  frameSize = width * height * 3 * sizeof(float);
  bgr = (float *)malloc(frameSize);
  if (bgr == NULL) {
    std::cerr << "Failed malloc for bgr\n";
    return;
  }

  d_bgr = d_srcBGR;
  for (int i = 0; i < batchSize; i++) {
    char filename[120];
    std::ofstream *outputFile;

    checkCudaErrors(cudaMemcpy((void *)bgr, (void *)d_bgr, frameSize,
                               cudaMemcpyDeviceToHost));
    sprintf(filename, "%s/%s_%d.raw", directory, tag, (i + 1));

    outputFile = new std::ofstream(filename);
    if (outputFile) {
      outputFile->write((char *)bgr, frameSize);
      delete outputFile;
    }

    d_bgr += pitch * height * 3;
  }

  free(bgr);
}

void dumpBGR(float *d_srcBGR, int pitch, int width, int height, int batchSize,
             char *folder, char *tag) {
  dumpRawBGR(d_srcBGR, pitch, width, height, batchSize, folder, tag);
}

void dumpYUV(unsigned char *d_nv12, int size, char *folder, char *tag) {
  unsigned char *nv12Data;
  std::ofstream *nv12File;
  char filename[120];
  char directory[60];
  char mkdir_cmd[256];
#if !defined(_WIN32)
  sprintf(directory, "output/%s", folder);
  sprintf(mkdir_cmd, "mkdir -p %s 2> /dev/null", directory);
#else
  sprintf(directory, "output\\%s", folder);
  sprintf(mkdir_cmd, "mkdir %s 2> nul", directory);
#endif

  int ret = system(mkdir_cmd);

  sprintf(filename, "%s/%s.nv12", directory, tag);

  nv12File = new std::ofstream(filename);
  if (nv12File == NULL) {
    std::cerr << "Failed to new " << filename;
    return;
  }

  nv12Data = (unsigned char *)malloc(size * (sizeof(char)));
  if (nv12Data == NULL) {
    std::cerr << "Failed to allcoate memory\n";
    return;
  }

  cudaMemcpy((void *)nv12Data, (void *)d_nv12, size, cudaMemcpyDeviceToHost);

  nv12File->write((const char *)nv12Data, size);

  free(nv12Data);
  delete nv12File;
}
