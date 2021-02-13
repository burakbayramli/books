/*
 * Copyright 2016 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */


//
// DESCRIPTION:   Common egl stream functions
//

#include "eglstrm_common.h"

EGLStreamKHR eglStream;
EGLDisplay   g_display;
EGLAttrib    cudaIndex;

#if defined(EXTENSION_LIST)
EXTENSION_LIST(EXTLST_DECL)
typedef void (*extlst_fnptr_t)(void);
static struct {
    extlst_fnptr_t *fnptr;
    char const *name;
} extensionList[] = { EXTENSION_LIST(EXTLST_ENTRY) };

int eglSetupExtensions(void)
{
    unsigned int i;

    for (i = 0; i < (sizeof(extensionList) / sizeof(*extensionList)); i++) {
        *extensionList[i].fnptr = eglGetProcAddress(extensionList[i].name);
        if (*extensionList[i].fnptr == NULL) {
            printf("Couldn't get address of %s()\n", extensionList[i].name);
            return 0;
        }
    }

    return 1;
}

int EGLStreamInit(int *cuda_device)
{
    static const EGLint streamAttrMailboxMode[] = { EGL_SUPPORT_REUSE_NV, EGL_FALSE, EGL_NONE };
    EGLBoolean eglStatus;
#define MAX_EGL_DEVICES 4
    EGLint numDevices = 0;
    EGLDeviceEXT devices[MAX_EGL_DEVICES];
    eglStatus = eglQueryDevicesEXT(MAX_EGL_DEVICES, devices, &numDevices);
    if (eglStatus != EGL_TRUE) {
        printf("Error querying EGL devices\n");
        exit(EXIT_FAILURE);
    }

    if (numDevices == 0) {
        printf("No EGL devices found.. Waiving\n");
        eglStatus = EGL_FALSE;
        exit(EXIT_WAIVED);
    }

    int egl_device_id = 0;
    for(egl_device_id = 0; egl_device_id < numDevices; egl_device_id++)
    {
        eglStatus = eglQueryDeviceAttribEXT(devices[egl_device_id], EGL_CUDA_DEVICE_NV, &cudaIndex);
        if (eglStatus == EGL_TRUE)
        {
            *cuda_device = cudaIndex; // We select first EGL-CUDA Capable device.
            printf("Found EGL-CUDA Capable device with CUDA Device id = %d\n", (int)cudaIndex);
            break;
        }
    }

    if (egl_device_id >= numDevices)
    {
        printf("No CUDA Capable EGL Device found.. Waiving execution\n");
        exit(EXIT_WAIVED);
    }

    g_display = eglGetPlatformDisplayEXT(EGL_PLATFORM_DEVICE_EXT, (void*)devices[egl_device_id], NULL); 
    if (g_display == EGL_NO_DISPLAY) {
        printf("Could not get EGL display from device. \n");
        eglStatus = EGL_FALSE;
        exit(EXIT_FAILURE);
    }

    eglStatus = eglInitialize(g_display, 0, 0);
    if (!eglStatus) {
        printf("EGL failed to initialize. \n");
        eglStatus = EGL_FALSE;
        exit(EXIT_FAILURE);
    }

    eglStream = eglCreateStreamKHR(g_display, streamAttrMailboxMode);
    if (eglStream == EGL_NO_STREAM_KHR) {
        printf("Could not create EGL stream.\n");
        eglStatus = EGL_FALSE;
        exit(EXIT_FAILURE);
    }

    printf("Created EGLStream %p\n", eglStream);

    // Set stream attribute
    if(!eglStreamAttribKHR(g_display, eglStream, EGL_CONSUMER_LATENCY_USEC_KHR, 16000)) {
        printf("Consumer: eglStreamAttribKHR EGL_CONSUMER_LATENCY_USEC_KHR failed\n");
        return 0;
    }
    if(!eglStreamAttribKHR(g_display, eglStream, EGL_CONSUMER_ACQUIRE_TIMEOUT_USEC_KHR, 16000)) {
        printf("Consumer: eglStreamAttribKHR EGL_CONSUMER_ACQUIRE_TIMEOUT_USEC_KHR failed\n");
        return 0;
    }
    printf("EGLStream initialized\n");
    return 1;
}

void EGLStreamFini(void)
{
    eglDestroyStreamKHR(g_display, eglStream);
}
#endif
