#include "Global_var.h"
#include "sub_NS_singlegid.h"
#include "sub_Residual.h"
#include "sub_boundary.h"
#include "Flow_Var.h"
#include "sub_LU_SGS.h"
#include "sub_NS_multigrid.h"
#include <stdio.h>
#include <cmath>
#include <ctime>
#include <fstream>

void force_vt_kw(int nMesh);

//在给定的网格上求解N - S方程 （推进1个时间步）
//对于单重网格，nMesh = 1;  对于多重网格，nMesh = 1, 2, 3, ... 分别对应用细网格、粗网格、更粗网格 ...
void NS_Time_advance(int nMesh)
{
	if (Time_Method == Time_Euler1) {	 //1阶Euler
		NS_Time_advance_1Euler(nMesh);          
	}
	else if (Time_Method == Time_RK3) {		 //3阶RK
		//NS_Time_advance_RK3(nMesh);                      
	}
	else if (Time_Method == Time_LU_SGS) {			//LU - SGS隐格式
		NS_time_advance_LU_SGS(nMesh);
	}
	else if (Time_Method == Time_Dual_LU_SGS) {	//双时间步LU - SGS(不支持多重网格)
		//Dual_time_LU_SGS();
	}
	else {
		printf("This time advance method is not supported!!!\n");
	}

	//强制 k, w, vt 非负
	force_vt_kw(nMesh);
}


//强制 k, w, vt 非负
void force_vt_kw(int nMesh)
{
	Mesh_TYPE & MP = Mesh[nMesh];
	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {
		Block_TYPE & B = MP.Block[mBlock];
		int nx = B.nx;  int ny = B.ny;
		if (MP.Nvar == 5) {
			for (int i = 1+LAP; i <= nx - 1+LAP; ++i) {
				for (int j = 1+LAP; j <= ny - 1+LAP; ++j) {
					if (B.U[i][j][5] < 0)
						B.U[i][j][5] = 1.e-10;
				}
			}
		}
		else if (MP.Nvar == 6) {
			for (int i = 1 + LAP; i <= nx - 1 + LAP; ++i) {
				for (int j = 1 + LAP; j <= ny - 1 + LAP; ++j) {
					if (B.U[i][j][5] < 0)
						B.U[i][j][5] = 1.e-10;
					if (B.U[i][j][6] < 0)
						B.U[i][j][6] = 1.e-10;
				}
			}
		}
	}
}

//-------------------------------------------------------------------------------------------- -
//采用LU_SGS法进行时间推进一个时间步 （第nMesh重网格 的单重网格）
void NS_time_advance_LU_SGS(int nMesh)
{
	std::ofstream fcout1, fcout2;
	fcout1.open("time_consuming1.dat", std::ios::app);
	fcout2.open("time_consuming2.dat", std::ios::app);
	clock_t start,mid, finish;
	start = clock();

	Comput_Residual_one_mesh(nMesh);     //单重网格上计算残差

	mid = clock();

	double alfa1 = 0.e0; 
	Mesh_TYPE &	MP = Mesh[nMesh];
	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {

		du_LU_SGS_2D(nMesh, mBlock, alfa1);         //采用LU_SGS方法计算DU = U(n + 1) - U(n)

		finish = clock();

		//double duration = (double)(finish - start) / CLOCKS_PER_SEC;
		double dur1 = (double)(mid - start);
		double dur2 = (double)(finish - mid);
		//printf("time1= %f, time2=%f \n", dur1, dur2);
		fcout1 << dur1 << std::endl;
		fcout2<<"  " << dur2 << std::endl;
		fcout1.close();	//统计求解通量的时间和计算LU_SUS的时间
		fcout2.close();	//统计求解通量的时间和计算LU_SUS的时间

		Block_TYPE & B = MP.Block[mBlock];
		int nx = B.nx; int ny = B.ny;
		//--------------------------------------------------------------------------------------
		//时间推进
		for (int i = 1; i <= nx - 1; ++i) {
			for (int j = 1; j <= ny - 1; ++j) {
				for (int m = 1; m <= Nvar; ++m) {
					B.U[i+LAP][j + LAP][m] +=  B.dU[i][j][m];        //U(n + 1) = U(n) + dU
					//printf("\n B.U[%d][%d][%d] = %10.9e,  B.Res = %10.9e, dU=%10.9e \n", i, j, m, B.U[i + LAP][j + LAP][m], B.Res[i][j][m], B.dU[i][j][m]);
				}
			}
		}
	}
		
	//--------------------------------------------------------------------------------------
	Boundary_condition_onemesh(nMesh);        //边界条件 （设定Ghost Cell的值）
	update_buffer_onemesh(nMesh);         // 同步各块的交界区

	Mesh[nMesh].tt = Mesh[nMesh].tt + dt_global;      //时间 （使用全局时间步长法时有意义）
	Mesh[nMesh].Kstep = Mesh[nMesh].Kstep + 1;        //计算步数
}

//----------------------------------------------------------
//计算无粘性及粘性项的谱半径，在加速收敛技术（局部时间步长，隐格式，残差光顺等）中使用
void comput_Lvc(int nMesh, int mBlock, flow_var &fl)
{
	double C0, si, sj, s0, ni1, ni2, nj1, nj2, uni, unj, tmp1;
	Block_TYPE & B = Mesh[nMesh].Block[mBlock];                 //第nMesh 重网格的第mBlock块

	C0 = 1.E0;
	int nx = B.nx; int ny = B.ny;

	//$OMP PARALLEL for( int  DEFAULT(SHARED) PRIVATE(i, j, si, sj, s0, ni1, ni2, nj1, nj2, uni, unj, tmp1)
	for (int i = 1; i <= nx - 1; ++i) {
     	for (int j = 1; j <= ny - 1; ++j) {
			int myi = i + LAP;	int myj = j + LAP;
			s0 = B.vol[i][j];         //面积
			si = 0.5E0*(B.si[i][j] + B.si[i + 1][j]);        //控制体边长（两侧平均）
			ni1 = 0.5E0*(B.ni1[i][j] + B.ni1[i + 1][j]);      //界面法方向（两侧平均）
			ni2 = 0.5E0*(B.ni2[i][j] + B.ni2[i + 1][j]);
			sj = 0.5E0*(B.sj[i][j] + B.sj[i][j + 1]);
			nj1 = 0.5E0*(B.nj1[i][j] + B.nj1[i][j + 1]);
			nj2 = 0.5E0*(B.nj2[i][j] + B.nj2[i][j + 1]);
			uni = fl.uu[myi][myj] * ni1 + fl.v[myi][myj] * ni2;             //法向速度
			unj = fl.uu[myi][myj] * nj1 + fl.v[myi][myj] * nj2;

			//谱半径
			B.Lci[i][j] = (abs(uni) + fl.cc[i][j])*si;         //无粘项Jocabian矩阵的谱半径 （Blazek's Book 6.1.4节）
			B.Lcj[i][j] = (abs(unj) + fl.cc[i][j])*sj;

			tmp1 = gamma / fl.d[myi][myj] * (B.Amu[myi][myj] / Pr + B.Amu_t[myi][myj] / PrT);
			B.Lvi[i][j] = tmp1*si*si / s0;         //粘性项Jocabian矩阵谱半径
			B.Lvj[i][j] = tmp1*sj*sj / s0;
		}
	}
	//$OMP END PARALLEL for( int 
}


//计算（当地）时间步长
void comput_dt(int nMesh, int mBlock)
{
	const double C0 = 1;
	Block_TYPE  & B = Mesh[nMesh].Block[mBlock];                 //第nMesh 重网格的第mBlock块
	int nx = B.nx; int ny = B.ny;

	//$OMP PARALLEL for( int DEFAULT(SHARED) PRIVATE[i][j]
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			if (Iflag_local_dt == 0) {   //全局时间步长
				B.dt[i][j] = dt_global;
			}
			else
			{
				//当地时间步长
				B.dt[i][j] = CFL*B.vol[i][j] / (B.Lci[i][j] + B.Lcj[i][j] + C0*(B.Lvi[i][j] + B.Lvj[i][j]));     //局部时间步长
				if (B.dt[i][j]>dtmax) B.dt[i][j] = dtmax;
				if (B.dt[i][j]<dtmin) B.dt[i][j] = dtmin;
			}
		}
	}
	//$OMP END PARALLEL DO
}

//采用1阶Euler法进行时间推进一个时间步 （第nMesh重网格 的单重网格）
void NS_Time_advance_1Euler(int nMesh)
{
	Mesh_TYPE & MP = Mesh[nMesh];
	Comput_Residual_one_mesh(nMesh);     //单重网格上计算残差

	if (nMesh != 1) Add_force_function(nMesh);   //添加强迫函数（多重网格的粗网格使用）

	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {

		Block_TYPE & B = MP.Block[mBlock];
		int nx = B.nx; int ny = B.ny;
		//------------------------------------------------------------------------------
		//时间推进
		//$OMP PARALLEL for(int  DEFAULT(SHARED) PRIVATE(i, j, m, du)
		for (int i = 1; i <= nx - 1; ++i) {
			for (int j = 1; j <= ny - 1; ++j) {
				for (int m = 1; m <= MP.Nvar; ++m) {
					double du = B.Res[i][j][m] / B.vol[i][j];
					B.U[i+LAP][j+LAP][m] += B.dt[i][j] * du;
					//printf("\n\n%d %d \n du= %f,Res= %f,VOL= %f\n dt= %f, U=%f",i,j, du, B.Res[i][j][m], B.vol[i][j], B.dt[i][j], B.U[i + LAP][j + LAP][m]);
				}
			}
		}
		//$OMP do PARALLEL do 
	}

	//-------------------------------------------------------------------------------------- -
	//-------------------------------------------------------------------------------------- -
	Boundary_condition_onemesh(nMesh);         //边界条件 （设定Ghost Cell的值）
	update_buffer_onemesh(nMesh);        //同步各块的交界区

	Mesh[nMesh].tt = Mesh[nMesh].tt + dt_global;      //时间 （使用全局时间步长法时有意义）
	Mesh[nMesh].Kstep = Mesh[nMesh].Kstep + 1;        //计算步数
													  //print*, "Kstep, t=", Mesh(nMesh) . Kstep, Mesh(nMesh) . tt
	//outputDebug();
}