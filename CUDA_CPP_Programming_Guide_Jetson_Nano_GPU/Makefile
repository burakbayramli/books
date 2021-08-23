###############################################################################
#
# Copyright 1993-2015 NVIDIA Corporation.  All rights reserved.
#
# NOTICE TO USER:   
#
# This source code is subject to NVIDIA ownership rights under U.S. and 
# international Copyright laws.  
#
# NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE 
# CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR 
# IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH 
# REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF 
# MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.   
# IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL, 
# OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS 
# OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
# OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE 
# OR PERFORMANCE OF THIS SOURCE CODE.  
#
# U.S. Government End Users.  This source code is a "commercial item" as 
# that term is defined at 48 C.F.R. 2.101 (OCT 1995), consisting  of 
# "commercial computer software" and "commercial computer software 
# documentation" as such terms are used in 48 C.F.R. 12.212 (SEPT 1995) 
# and is provided to the U.S. Government only as a commercial end item.  
# Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through 
# 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the 
# source code with only those rights set forth herein.
#
###############################################################################
#
# CUDA Samples
#
###############################################################################

TARGET_ARCH ?= $(shell uname -m)

# Project folders that contain CUDA samples
PROJECTS ?= $(shell find 0_Simple 1_Utilities 2_Graphics 3_Imaging 4_Finance 5_Simulations 6_Advanced 7_CUDALibraries -name Makefile)

FILTER_OUT :=

ifeq ($(TARGET_ARCH),ppc64le)
FILTER_OUT += 2_Graphics/simpleGLES_screen/Makefile
FILTER_OUT += 5_Simulations/nbody_screen/Makefile
endif

PROJECTS := $(filter-out $(FILTER_OUT),$(PROJECTS))

%.ph_build :
	+@$(MAKE) -C $(dir $*) $(MAKECMDGOALS)

%.ph_clean : 
	+@$(MAKE) -C $(dir $*) clean $(USE_DEVICE)

%.ph_clobber :
	+@$(MAKE) -C $(dir $*) clobber $(USE_DEVICE)

all:  $(addsuffix .ph_build,$(PROJECTS))
	@echo "Finished building CUDA samples"

build: $(addsuffix .ph_build,$(PROJECTS))

tidy:
	@find * | egrep "#" | xargs rm -f
	@find * | egrep "\~" | xargs rm -f

clean: tidy $(addsuffix .ph_clean,$(PROJECTS))

clobber: clean $(addsuffix .ph_clobber,$(PROJECTS))
