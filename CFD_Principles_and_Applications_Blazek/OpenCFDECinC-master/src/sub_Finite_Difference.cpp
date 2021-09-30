//用差分法计算通量 （相当于补丁程序）
//多重网格情况下，仅在最密的网格采用 （稀网格通常不采用高精度方法）

#include "Global_var.h"
#include "sub_Finite_Difference.h"
#include "common.h"
#include <fstream>
#include<iomanip>

void comput_Jacobin();


//有限差分模块的初始化
void Init_FiniteDifference()
{
	std::ifstream fcin;
	fcin.open("FDM.in");
	if (!fcin) {
		printf("Not find 'FDM.in', all blocks using Finite Volume Method\n");
		//PAUSE;
		//exit(1);
	}
	else {

	}

	comput_Jacobin();
}

//
void comput_Jacobin()
{
	std::ofstream fcout;
	fcout.open("");
	fcout << std::setprecision(12);
	fcout << "variables=x,y,kx,ky,ix,iy,Jac,s1" << std::endl;
	
	for (int m = 1; m <= Mesh[1].Num_Block; ++m) {
		Block_TYPE &B = Mesh[1].Block[m];
		if (B.FVM_FDM != Method_FDM) { continue; }
		
		// 这个地方的程序没有加入
		printf("can not computer the Jacobin cooeficient!\n");
		PAUSE;
	}




	fcout.close();
}
