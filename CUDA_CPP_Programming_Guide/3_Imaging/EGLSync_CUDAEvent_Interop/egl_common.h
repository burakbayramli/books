/*
 * egl_common.h
 *
 * Copyright (c) 2017, NVIDIA CORPORATION. All rights reserved.
 *
 * NVIDIA CORPORATION and its licensors retain all intellectual property
 * and proprietary rights in and to this software, related documentation
 * and any modifications thereto.  Any use, reproduction, disclosure or
 * distribution of this software and related documentation without an express
 * license agreement from NVIDIA CORPORATION is strictly prohibited.
 */

//
// DESCRIPTION:   Common EGL functions header file
//

#ifndef _EGL_COMMON_H_
#define _EGL_COMMON_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <signal.h>
#include "cuda.h"
#include "cudaEGL.h"

EGLImageKHR eglImage;

#define EXTENSION_LIST(T) \
    T( PFNEGLCREATEIMAGEKHRPROC,          eglCreateImageKHR ) \
    T( PFNEGLDESTROYIMAGEKHRPROC,         eglDestroyImageKHR ) \
    T( PFNEGLCREATESYNCKHRPROC,           eglCreateSyncKHR ) \
    T( PFNEGLDESTROYSYNCKHRPROC,          eglDestroySyncKHR ) \
    T( PFNEGLCLIENTWAITSYNCKHRPROC,       eglClientWaitSyncKHR ) \
    T( PFNEGLGETSYNCATTRIBKHRPROC,        eglGetSyncAttribKHR ) \
    T( PFNEGLCREATESYNC64KHRPROC,         eglCreateSync64KHR ) \
    T( PFNEGLWAITSYNCKHRPROC,             eglWaitSyncKHR )

#define eglCreateImageKHR                 my_eglCreateImageKHR
#define eglDestroyImageKHR                my_eglDestroyImageKHR
#define eglCreateSyncKHR                  my_eglCreateSyncKHR
#define eglDestroySyncKHR                 my_eglDestroySyncKHR
#define eglClientWaitSyncKHR              my_eglClientWaitSyncKHR
#define eglGetSyncAttribKHR               my_eglGetSyncAttribKHR
#define eglCreateSync64KHR                my_eglCreateSync64KHR
#define eglWaitSyncKHR                    my_eglWaitSyncKHR


#define EXTLST_DECL(tx, x)  tx my_ ## x = NULL;
#define EXTLST_EXTERN(tx, x) extern tx my_ ## x;
#define EXTLST_ENTRY(tx, x) { (extlst_fnptr_t *)&my_ ## x, #x },

int eglSetupExtensions(void);
#endif
