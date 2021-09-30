#include "sub_NS_multigrid.h"
#include "Global_Var.h"
#include<stdio.h>

//----------------------------------------------------------------------
//粗网格向细网格的插值(Prolong) 及 细网格向粗网格上插值(interpolation)
//----------------------------------------------------------------------
//将网格m1的守恒变量(U) 或U的差插值到网格m2(上一级细网格)
//flag = 1时，将U插值到上一级网格；(准备初值时使用)
//flag = 2时，将deltU插值到上一级网格 （deltU储存着本时间步与上个时间步U的差）

void prolong_U(int m1, int m2, int flag)
{
	printf("this prolong file is undefined\n");
	
	//system("pause");

}

//------------------------------------------------------------
//把强迫函数添加到残差中 RF = R + QF
void Add_force_function(int nMesh)
{
	for (int mBlock = 1; Mesh[nMesh].Num_Block; ++mBlock) {
		Block_TYPE & B = Mesh[nMesh].Block[mBlock];
		//$OMP PARALLEL for( int DEFAULT(SHARED) PRIVATE(i, j, m)
		for (int i = 1; i <= B.nx - 1; ++i) {
			for (int j = 1; j <= B.ny - 1; ++j) {
				for (int m = 1; m <= 4; ++m) {
					B.Res[i][j][m] = B.Res[i][j][m] + B.QF[i][j][m];            //添加强迫函数后的残差仍储存在B.Res里面 （节省内存）
				}
			}
		}
		//$OMP END PARALLEL for( int
	}
}
