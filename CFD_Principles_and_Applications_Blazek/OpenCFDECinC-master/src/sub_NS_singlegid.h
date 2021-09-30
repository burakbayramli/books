#pragma once
#ifndef _SUB_NS_SINGLEGRID_H_
#define _SUB_NS_SINGLEGRID_H_
#include "Flow_Var.h"

void NS_Time_advance(int nMesh);

void NS_time_advance_LU_SGS(int nMesh);
void NS_Time_advance_1Euler(int nMesh);


void comput_Lvc(int nMesh, int mBlock, flow_var &fl);

void comput_dt(int nMesh, int mBlock);

#endif // !_SUB_NS_SINGLEGRID_H_
