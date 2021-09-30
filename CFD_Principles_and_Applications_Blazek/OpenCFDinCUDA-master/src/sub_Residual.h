#pragma once
#ifndef _sub_Residual_h_
#define _sub_Residual_h_

#include "Flow_var.h"

void Comput_Residual_one_mesh(int nMesh);

void get_viscous(int nMesh, int mBlock, flow_var &fl);





#endif // !_sub_Residual_h_

