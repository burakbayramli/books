/*
 * Copyright 1993-2018 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

#ifndef _COMMON_DEFS_
#define _COMMON_DEFS_
#include <cuda.h>

#define ONE_KB 1024
#define ONE_MB (ONE_KB * ONE_KB)

extern size_t maxSampleSizeInMb;
extern int numKernelRuns;
extern int verboseResults;

extern unsigned int findNumSizesToTest(unsigned int minSize,
                                       unsigned int maxSize,
                                       unsigned int multiplier);

// For Tracking the different memory allocation types
typedef enum memAllocType_enum {
  MEMALLOC_TYPE_START,
  USE_MANAGED_MEMORY_WITH_HINTS = MEMALLOC_TYPE_START,
  USE_MANAGED_MEMORY_WITH_HINTS_ASYNC,
  USE_MANAGED_MEMORY,
  USE_ZERO_COPY,
  USE_HOST_PAGEABLE_AND_DEVICE_MEMORY,
  USE_HOST_PAGEABLE_AND_DEVICE_MEMORY_ASYNC,
  USE_HOST_PAGELOCKED_AND_DEVICE_MEMORY,
  USE_HOST_PAGELOCKED_AND_DEVICE_MEMORY_ASYNC,
  MEMALLOC_TYPE_END = USE_HOST_PAGELOCKED_AND_DEVICE_MEMORY_ASYNC,
  MEMALLOC_TYPE_INVALID,
  MEMALLOC_TYPE_COUNT = MEMALLOC_TYPE_INVALID
} MemAllocType;

typedef enum bandwidthType_enum {
  READ_BANDWIDTH,
  WRITE_BANDWIDTH
} BandwidthType;

extern const char *memAllocTypeStr[];
extern const char *memAllocTypeShortStr[];

struct resultsData;
struct testResults;

void createAndInitTestResults(struct testResults **results,
                              const char *testName,
                              unsigned int numMeasurements,
                              unsigned int numSizesToTest);
unsigned long *getPtrSizesToTest(struct testResults *results);

void freeTestResultsAndAllResultsData(struct testResults *results);

void createResultDataAndAddToTestResults(struct resultsData **ptrData,
                                         struct testResults *results,
                                         const char *resultsName,
                                         bool printOnlyInVerbose,
                                         bool reportAsBandwidth);
double *getPtrRunTimesInMs(struct resultsData *data, int allocType,
                           int sizeIndex);

void printResults(struct testResults *results,
                  bool print_launch_transfer_results, bool print_std_deviation);
#endif
