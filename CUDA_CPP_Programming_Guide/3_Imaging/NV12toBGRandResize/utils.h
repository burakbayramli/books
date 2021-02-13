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

#ifndef __H_UTIL_
#define __H_UTIL_

extern "C"
void dumpBGR(float *d_srcBGR, int pitch, int width, int height,
              int batchSize, char *folder, char *tag);
extern "C"
void dumpYUV(unsigned char *d_nv12, int size, char *folder, char *tag);
#endif
