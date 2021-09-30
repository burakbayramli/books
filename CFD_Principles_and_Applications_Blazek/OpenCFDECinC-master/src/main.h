#pragma once
#ifndef _MAIN_H_
#define _MAIN_H_

//----------------------------------------------------------------------------
//流场物理量 （计算每块时申请内存，该块计算结束后释放；属于临时变量）
//这些物理量的值无需保留;
/*
module Flow_Var
real * 8, save, pointer, dimension(:, : )::d, uu, v, T, p, cc  //密度、x - 速度、y - 速度、压力、声速；
real * 8, save, pointer, dimension(:, : , : )::Fluxi, Fluxj         //i - 及j - 方向的通量
end module Flow_Var
*/

void output_Res(int nMesh);
void output(int nMesh);


#endif // !_MAIN_H_
