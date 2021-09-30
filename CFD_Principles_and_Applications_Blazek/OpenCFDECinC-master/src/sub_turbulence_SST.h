#pragma once
#ifndef _TURBULENCE_SST_H_
#define _TURBULENCE_SST_H_

#include "Flow_Var.h"

void comput_dw();

void Amut_boundary(int nMesh, int mBlock);

void SST_kw(int nMesh, int mBlock, flow_var & fl);


#endif // !_TURBULENCE_SST_H_

