//采用LU - SGS方法，计算DU = U(n + 1) - U(n)

#include <stdlib.h>
#include<stdio.h>
#include "sub_LU_SGS.h"
#include "Global_var.h"
#include "common.h"


void comput_DFn(int NV, double*  DF, double U[], double DU[], double n1, double  n2, double gamma);

__global__
void sweep(int offset, int * transferInt, double *transferDouble, double * dU)
{


}

//采用LU_SGS方法计算DU = U(n + 1) - U(n)
void  du_LU_SGS_2D(int nMesh, int mBlock, double alfa1)
{
	double alfa[7], dui[7], duj[7], DF[7];   //alfa, 对角线元素值;  dui, duj, DF对流通量
	const double w_LU = 1.E0;   //LU_SGS的松弛因子(1 - 2之间), 增大w_LU会提高稳定性，但会降低收敛速度
	alfa1;                 //双时间步LU_SGS附加对角线值
	Mesh_TYPE & MP = Mesh[nMesh];
	int NV = MP.Nvar;
	Block_TYPE & B = MP.Block[mBlock];                 //第nMesh 重网格的第mBlock块
	int nx = B.nx; int ny = B.ny;

	//LU - SGS的两次扫描
	//----------------------------------
	//从i = 1, j = 1 到i = nx - 1, j = ny - 1的扫描过程(向上扫描过程)
	//扫描 i + j = plane 的平面
	//w_LU是松弛因子（1到2之间），增大w_LU会提高稳定性，但会降低收敛速度
	for (int plane = 2; plane <= nx + ny - 2; ++plane) {
		//$OMP PARALLEL for( int DEFAULT(PRIVATE) SHARED(plane, nx, ny, NV, B, gamma, If_viscous, alfa1)
		int maxCirculation = plane - 1 < ny - 1 ? plane + 1 : ny - 1;
		int minCirculation = plane - nx + 1 > 1 ? plane - nx + 1 : 1;
		int offset = minCirculation;
		int total_threads = maxCirculation - minCirculation + 1;
		int thread = 32;
		int block = (total_threads + thread-1) / total_threads;

		sweep << <block, thread >> > (offset, transferInt_dev, transferDouble_dev, dU);
		for (int j = 1; j <= ny - 1; ++j) {
			int i = plane - j;
			if (i< 1 || i>nx - 1) { continue; }    //超出了这个平面
												   //计算对接线元素 （如考虑粘性则再加上粘性谱半径； 对于湍流模型中的方程，则增加源项谱半径）
			for (int m = 1; m <= 6; ++m) {
				alfa[m] = B.vol[i][j] / B.dt[i][j] + w_LU*(B.Lci[i][j] + B.Lcj[i][j]) + B.vol[i][j] * alfa1;        //对角元
			}

			if (If_viscous == 1) {
				for (int m = 1; m <= 6; ++m) {
					alfa[m] += 2.E0*(B.Lvi[i][j] + B.Lvj[i][j]);           //加上粘性项
				}
			}
			if (NV == 5) {
				alfa[5] = alfa[5] + B.Lvi[i][j] + B.Lvj[i][j];                //SA模型
																			  //}elseif(NV== 6) {                                 //SST模型
																			  //alfa(5) = alfa(5) + 0.09*B.U(6[i][j] / B.U(1[i][j]
																			  //alfa(6) = alfa(6) + 2.E0*0.0828*B.U(6[i][j] / B.U(1[i][j]
			}

			if (i != 1) {
				//通量的差量，用来近似计算A*W(See Blazek's book, page 208)
				comput_DFn(NV, DF, B.U[i - 1 + LAP][j + LAP], B.dU[i - 1][j], B.ni1[i][j], B.ni2[i][j], gamma);

				for (int m = 1; m <= NV; ++m) {
					dui[m] = 0.5E0*(DF[m] * B.si[i][j] + w_LU*B.Lci[i - 1][j] * B.dU[i - 1][j][m]);
				}

				if (If_viscous == 1) {
					for (int m = 1; m <= NV; ++m) {
						dui[m] = dui[m] + B.Lvi[i - 1][j] * B.dU[i - 1][j][m];         //2012 - 2 - 29, 稳定性更好些
					}
				}

			}
			else {
				for (int m = 1; m <= NV; ++m) {
					dui[m] = 0.E0;                             //左侧没有点
				}
			}

			if (j != 1) {
				comput_DFn(NV, DF, B.U[i+LAP][j - 1+LAP], B.dU[i][j - 1], B.nj1[i][j], B.nj2[i][j], gamma);
				for (int m = 1; m <= NV; ++m) {
					duj[m] = 0.5E0*(DF[m] * B.sj[i][j] + w_LU*B.Lcj[i][j - 1] * B.dU[i][j - 1][m]);
				}

				if (If_viscous == 1) {
					for (int m = 1; m <= NV; ++m) {
						duj[m] = duj[m] + B.Lvj[i][j - 1] * B.dU[i][j - 1][m];    //2012 - 2 - 29
					}
				}
			}
			else {
				for (int m = 1; m <= NV; ++m) {
					duj[m] = 0.E0;
				}
			}
			//printf("%d , %d \n", i, j);
			for (int m = 1; m <= NV; ++m) {
				B.dU[i][j][m] = (B.Res[i][j][m] + dui[m] + duj[m]) / alfa[m];
				//printf("%f,  %f,  %f,  %f,   %f\n", B.dU[i][j][m], B.Res[i][j][m], dui[m], duj[m], alfa[m]);
			}
			//PAUSE;
		}
		//$OMP END PARALLEL do int
	}
	//----------------------------------------------------------
	//从(nx - 1, ny - 1)到(1, 1)的扫描过程 （向下扫描过程）
	//plane = i + j + k
	for (int plane = nx + ny - 2; plane >= 2; --plane) {

		//$OMP PARALLEL for( int DEFAULT(PRIVATE) SHARED(plane, nx, ny, NV, B, gamma, If_viscous, alfa1)
		for (int j = ny - 1; j >= 1; --j) {
			int i = plane - j;
			if (i< 1 || i>nx - 1) { continue; }            //超出了这个平面
			for (int m = 1; m <= 6; ++m) {
				alfa[m] = B.vol[i][j] / B.dt[i][j] + w_LU*(B.Lci[i][j] + B.Lcj[i][j]) + B.vol[i][j] * alfa1;
			}
			if (If_viscous == 1) {
				for (int m = 1; m <= 6; ++m) {
					alfa[m] += 2.E0*(B.Lvi[i][j] + B.Lvj[i][j]);
				}
			}
			if (NV == 5) {
				alfa[5] = alfa[5] + B.Lvi[i][j] + B.Lvj[i][j];
				//}elseif(NV== 6) {
				//alfa(5) = alfa(5) + 0.09*B.U(6[i][j] / B.U(1[i][j]
				//alfa(6) = alfa(6) + 2.E0*0.0828*B.U(6[i][j] / B.U(1[i][j]
			}

			if (i != nx - 1) {
				//通量的差量，用来近似计算A*W(See Blazek's book, page 208)
				comput_DFn(NV, DF, B.U[i + 1+LAP][j+LAP], B.dU[i + 1][j], B.ni1[i][j], B.ni2[i][j], gamma);
				for (int m = 1; m <= NV; ++m) {
					dui[m] = -0.5E0*(DF[m] * B.si[i + 1][j] - w_LU*B.Lci[i + 1][j] * B.dU[i + 1][j][m]);
				}
				if (If_viscous == 1) {
					for (int m = 1; m <= NV; ++m) {
						dui[m] += B.Lvi[i + 1][j] * B.dU[i + 1][j][m];
					}
				}
			}
			else {
				for (int m = 1; m <= NV; ++m) {
					dui[m] = 0.E0;
				}
			}

			if (j != ny - 1) {
				comput_DFn(NV, DF, B.U[i+LAP][j + 1+LAP], B.dU[i][j + 1], B.nj1[i][j], B.nj2[i][j], gamma);
				for (int m = 1; m <= NV; ++m) {
					duj[m] = -0.5E0*(DF[m] * B.sj[i][j + 1] - w_LU*B.Lcj[i][j + 1] * B.dU[i][j + 1][m]);
				}
				if (If_viscous == 1) {
					for (int m = 1; m <= NV; ++m) {
						duj[m] = duj[m] + B.Lvj[i][j + 1] * B.dU[i][j + 1][m];
					}
				}
			}
			else {
				for (int m = 1; m <= NV; ++m) {
					duj[m] = 0.E0;
				}
			}

			for (int m = 1; m <= NV; ++m) {
				B.dU[i][j][m] += (dui[m] + duj[m]) / alfa[m];
			}
		}
		//$OMP END PARALLEL for( int
	}
}


//计算通量的差量 DF = F(U + DU) - F(U), LU - SGS方法中使用，用来近似A*DU
//comput_DFn(NV, DF, B.U[i - 1 + LAP][j + LAP], B.dU[i - 1][j], B.ni1[i][j], B.ni2[i][j], gamma);
void comput_DFn(int NV, double*  DF, double U[], double DU[], double n1, double  n2, double gamma)
{
	double * U2=(double *) malloc((NV+1)*sizeof(double));
	for (int m = 1; m <= NV; ++m) {
		U2[m] = U[m] + DU[m];		//新的守恒变量
		//printf("U2= %f    %d \n", U2[m], m);
	}
	double un1 = (U[2] * n1 + U[3] * n2) / U[1];      //un 法向速度
	double p1 = (gamma - 1.e0)*(U[4] - 0.5e0*(U[2] * U[2] + U[3] * U[3]) / U[1]);   //压力
	double un2 = (U2[2]*n1 + U2[3]*n2) / U2[1];       //法向速度un
	double p2 = (gamma - 1.e0)*(U2[4] - 0.5e0*(U2[2] * U2[2] + U2[3] * U2[3]) / U2[1]);   //压力
	
	//通量之差 DF = F(U + DU) - F(U)
	DF[1] = U2[1] * un2 - U[1] * un1;                  //d*un
	DF[2] = (U2[2] * un2 + p2*n1) - (U[2] * un1 + p1*n1);
	DF[3] = (U2[3] * un2 + p2*n2) - (U[3] * un1 + p1*n2);
	DF[4] = (U2[4] + p2) * un2 - (U[4] + p1) * un1;
	if (NV == 5) {
		DF[5] = U2[5] * un2 - U[5] * un1;
	}
	else if(NV ==  6){
		DF[5] = U2[5] * un2 - U[5] * un1;   //k方程的对流通量
		DF[6] = U2[6] * un2 - U[6] * un1;   //w方程的对流通量
	}
	free(U2);
}
