#include "kernelResidual.h"
#include "sub_Residual.h"
#include "common.h"
#include "Global_var.h"
#include "Flow_var.h"
#include"turbulence_SST.cuh"
#include<cstring>
#include <cmath>

__global__
void getResidual_i_WithKernel(int * transferInt, double *transferDouble, double* U, double * Amu, double *Amu_t,
	double * x1, double *y1, double *x, double *y, double *Fluxi, double *uu, double *vv, double * T);

__global__
void getResidual_j_WithKernel(int * transferInt, double *transferDouble, double* U, double * Amu, double *Amu_t,
	double * x1, double *y1, double *x, double *y, double *Fluxj, double *uu, double *vv, double * T);

__global__
void comput_duvtpckw_with_cuda(int * transferInt, double *transferDouble,  double * d,
	double * uu, double * vv, double *T, double * U);


__global__
void get_viscous_with_cuda(int * transferInt, double *transferDouble, double *Amu, double * T);

__device__
void cuda_scheme_fm(double & UR, double u1, double u2, double u3, double u4);

__device__
void cuda_scheme_fP(double &UL, double u1, double u2, double u3, double u4);

__device__
void cuda_Reconstuction_Characteristic(double U0[5][5], double *UL, double * UR, double gamma);

__device__
void  cuda_Flux_Van_Leer_1Da(double *QL, double * QR, double *Flux, const double gamma);

__device__
double  minmod(double a, double b)
{
	double  minmod;
	if (a*b < 0) {
		minmod = 0;
	}
	else {
		minmod = abs(a) <= abs(b) ? a : b;
	}
	return minmod;
}

__global__
void checkNum(int hi, int hj, double * vector, int colum)
{
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;

	if (i == hi && j == hj) {
		printf("Gpu= %d, %d, %f\n", i, j, vector[i*colum + j]);
	}
}

__global__
void checkNum(int hi, int hj,int k, double * vector, int colum, int zco)
{
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;

	if (i == hi && j == hj) {
		printf("GPU=  %d, %d, %f\n", i, j, vector[i*colum*zco + j*zco+k]);
	}
}

void beforeKernelResidual(int mBlock, flow_var  & fl)
{
	HANDLE_ERROR(cudaSetDevice(0));

	Block_TYPE & B = Mesh[1].Block[mBlock];
	const int nx = B.nx;	const int ny = B.ny;
	
	dim3 threadPerBlock(16, 16);
	dim3  blockPerGrid((nx + 2 * LAP + 1 + threadPerBlock.x - 1) / threadPerBlock.x, (ny + 2 * LAP + 1 + threadPerBlock.y - 1) / threadPerBlock.y);

	//将守恒变量传递到GPU上
	const int mm1 = nx + 2 * LAP - 1;	const int nn1 = ny + 2 * LAP - 1;
	double * U_dev;	
	HANDLE_ERROR(cudaMalloc((double **)& U_dev, (mm1 + 1)*(nn1 + 1) * 5 * sizeof(double)));
	double * U_host;	//主机上的页锁定内存
	HANDLE_ERROR(cudaHostAlloc((double **)&U_host, (mm1 + 1)*(nn1 + 1) * 5 * sizeof(double), cudaHostAllocDefault));
	for (int i = 1; i <= mm1; ++i) {
		for (int j = 1; j <= nn1; ++j) {
				memcpy(U_host + i*(nn1 + 1) * 5 + j * 5, B.U[i][j], 5 * sizeof(double));
		}
	}

	HANDLE_ERROR(cudaMemcpy(U_dev, U_host, (mm1 + 1)*(nn1 + 1) * 5 * sizeof(double), cudaMemcpyHostToDevice));

	//for (int i = 1; i <= mm1; ++i) {
	//	for (int j = 1; j <= nn1; ++j) {
	//		for (int k = 1; k <= 4; ++k) {
	//			checkNum << <blockPerGrid, threadPerBlock >> > (i, j, k, U_dev, nn1 + 1, 5);
	//			printf("CPU Host =  %f ",U_host[i*(nn1+1)*5+j*5+k]);
	//			printf("CPU = %d, %d, %f ",i, j, B.U[i][j][k]);
	//			cudaThreadSynchronize();
	//		}
	//	}
	//}

	HANDLE_ERROR(cudaFreeHost(U_host));		U_host = nullptr;

	//计算基本量
	double *d, double *uu; double *vv; double *T; 
	HANDLE_ERROR(cudaMalloc((double **)& d, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& uu, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& vv, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& T, (mm1 + 1)*(nn1 + 1) * sizeof(double)));

	comput_duvtpckw_with_cuda<<<blockPerGrid, threadPerBlock>>> (transferInt_dev, transferDouble_dev, d, uu, vv, T, U_dev);	//启动kernel 函数计算基本量

	//{//这里不在设备端计算基本量，而在主机端计算基本量，将数据拷贝回主机端
	//	double * temp_host;
	//	HANDLE_ERROR(cudaHostAlloc((double **)& temp_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaHostAllocDefault));
	//	const int colum_size = nn1 + 1;
	//	const int colum_size_bite = (nn1+1) * sizeof(double);
	//	//copy d
	//	for (int i = 0; i <= mm1; ++i) {
	//		memcpy( temp_host + i*colum_size, fl.d[i], colum_size_bite);
	//	}
	//	HANDLE_ERROR(cudaMemcpy(d, temp_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	//	//copy uu
	//	for (int i = 0; i <= mm1; ++i) {
	//		memcpy(temp_host + i*colum_size, fl.uu[i], colum_size_bite);
	//	}
	//	HANDLE_ERROR(cudaMemcpy(uu, temp_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	//	//copyt vv
	//	for (int i = 0; i <= mm1; ++i) {
	//		memcpy(temp_host + i*colum_size, fl.v[i], colum_size_bite);
	//	}
	//	HANDLE_ERROR(cudaMemcpy(vv, temp_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	//	//copy T
	//	for (int i = 0; i <= mm1; ++i) {
	//		memcpy(temp_host + i*colum_size, fl.T[i], colum_size_bite);
	//	}
	//	HANDLE_ERROR(cudaMemcpy(T, temp_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));

	///*	printf("%e ,%e ,%e ,%e\n", temp_host[4 * (nn1 + 1) + 4], fl.uu[4][4], temp_host[30 * (nn1 + 1) + 15], fl.uu[30][15]);
	//	PAUSE;*/
	//	HANDLE_ERROR(cudaFreeHost(temp_host));
	//}

	HANDLE_ERROR(cudaDeviceSynchronize());
	HANDLE_ERROR(cudaGetLastError());

	//计算层流粘性系数
	double *Amu_dev;
	HANDLE_ERROR(cudaMalloc((double **) & Amu_dev, (mm1+1)*(nn1+1)*sizeof(double)));
	//get_viscous_with_cuda << <blockPerGrid, threadPerBlock >> > (transferInt_dev, transferDouble_dev, Amu_dev, T);

	//计算湍流粘性系数
	double * Amu_t_dev;
	HANDLE_ERROR(cudaMalloc((double **)& Amu_t_dev, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMemset(Amu_t_dev, 0, (mm1 + 1)*(nn1 + 1) * sizeof(double)));

	//turbulence_SST_kw_before_cuda(Amu_t_dev, Amu_dev, d, uu, vv, T, U_dev, x_dev, y_dev, x1_dev, y1_dev, 
	//				mBlock, fl, transferInt_dev, transferDouble_dev); 

	HANDLE_ERROR(cudaDeviceSynchronize());	//同步
	HANDLE_ERROR(cudaGetLastError());

	//计算通量， 通量大小传递数组
	double * Fluxi_dev;	double * Fluxj_dev;
	HANDLE_ERROR(cudaMalloc((double **)& Fluxi_dev, (nx+1)*(ny+1)*5 * sizeof(double)));
	HANDLE_ERROR(cudaMemset(Fluxi_dev, 0, (nx + 1)*(ny + 1) * 5 * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& Fluxj_dev, (nx + 1)*(ny + 1) * 5 * sizeof(double)));
	HANDLE_ERROR(cudaMemset(Fluxj_dev, 0, (nx + 1)*(ny + 1) * 5 * sizeof(double)));

	double * Fluxi_host;
	HANDLE_ERROR(cudaHostAlloc((double **)&Fluxi_host, (nx + 1)*(ny + 1) * 5 * sizeof(double), cudaHostAllocDefault));

	double * Fluxj_host;
	HANDLE_ERROR(cudaHostAlloc((double **)&Fluxj_host, (nx + 1)*(ny + 1) * 5 * sizeof(double), cudaHostAllocDefault));


	//使用两个个流进行计算
	cudaStream_t first_stream;
	HANDLE_ERROR(cudaStreamCreate(&first_stream));
	cudaStream_t second_stream;
	HANDLE_ERROR(cudaStreamCreate(&second_stream));

	getResidual_i_WithKernel <<< blockPerGrid, threadPerBlock, 0, first_stream >>> ( transferInt_dev, transferDouble_dev,
								U_dev,Amu_dev, Amu_t_dev, x1_dev, y1_dev, x_dev, y_dev, Fluxi_dev, uu, vv, T);

	HANDLE_ERROR(cudaMemcpyAsync(Fluxi_host, Fluxi_dev, (nx + 1)*(ny + 1) * 5 * sizeof(double),cudaMemcpyDeviceToHost, first_stream));	//将通量拷贝到主机端

	getResidual_j_WithKernel << < blockPerGrid, threadPerBlock,0, second_stream >> > (transferInt_dev, transferDouble_dev,
								U_dev, Amu_dev, Amu_t_dev, x1_dev, y1_dev, x_dev, y_dev, Fluxj_dev, uu, vv, T);

	HANDLE_ERROR(cudaMemcpyAsync(Fluxj_host, Fluxj_dev, (nx + 1)*(ny + 1) * 5 * sizeof(double), cudaMemcpyDeviceToHost, second_stream));			//将通量拷贝到主机端

	HANDLE_ERROR(cudaDeviceSynchronize());	//同步
	HANDLE_ERROR(cudaGetLastError());

	for (int i = 1; i <= nx-1; ++i) {
		for (int j = 1; j <= ny-1; ++j) {
			for (int k = 1; k <= 4; ++k) {
				int a = i*(ny + 1) * 5 + j * 5 + k;
				int b = (i + 1)*(ny + 1) * 5 + j * 5 + k;
				int c = i*(ny + 1) * 5 + (j + 1) * 5 + k;
				B.Res[i][j][k] =Fluxi_host[b]-Fluxi_host[a] + Fluxj_host[c] - Fluxj_host[a];

				//printf("B.Res[%d][%d][%d]=  %15.12e , %15.12e , %15.12e\n", i, j, k, B.Res[i][j][k], Fluxi_host[b], Fluxi_host[a]);

				if (B.Res[i][j][k] != B.Res[i][j][k] || abs(B.Res[i][j][k])>10) {
					//printf("@@B.Res[%d][%d][%d]=  %15.12e \n", i, j, k, B.Res[i][j][k]);
					//PAUSE;
				}
			}
			//PAUSE;
		}
	}
	HANDLE_ERROR(cudaGetLastError());

	HANDLE_ERROR(cudaFreeHost(Fluxi_host));
	HANDLE_ERROR(cudaFreeHost(Fluxj_host));

	HANDLE_ERROR(cudaFree(Fluxi_dev)); HANDLE_ERROR(cudaFree(Fluxj_dev));
	HANDLE_ERROR(cudaFree(uu));	 HANDLE_ERROR(cudaFree(vv)); HANDLE_ERROR(cudaFree(T));
	//HANDLE_ERROR(cudaFree(x1_dev));	HANDLE_ERROR(cudaFree(y1_dev));
	//HANDLE_ERROR(cudaFree(x_dev));	HANDLE_ERROR(cudaFree(y_dev));
	HANDLE_ERROR(cudaFree(U_dev));
	HANDLE_ERROR(cudaFree(Amu_dev));	HANDLE_ERROR(cudaFree(Amu_t_dev));
	//HANDLE_ERROR(cudaFree(transferInt_dev));	HANDLE_ERROR(cudaFree(transferDouble_dev));
	
	HANDLE_ERROR(cudaStreamDestroy(first_stream));
	HANDLE_ERROR(cudaStreamDestroy(second_stream));
}

//计算 i 方向的数值通量
__global__
void getResidual_i_WithKernel(int * transferInt, double *transferDouble, double* U,double * Amu, double *Amu_t,
		double * x1, double *y1, double *x, double *y, double *tFluxi, double *uu, double *vv, double * T)
{
	__shared__  int  nx, ny, LAP, mm, nn, mm1, nn1;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];	ny = transferInt[1];
	LAP = transferInt[2];
	mm = nx + 2 * LAP; nn = ny + 2 * LAP;
	mm1 = nx + 2 * LAP-1; nn1 = ny + 2 * LAP-1;
	__shared__ double p00, gamma, Cp, Pr, PrT;
	p00 = transferDouble[0];
	gamma = transferDouble[1];
	Cp = transferDouble[6];
	Pr = transferDouble[7];
	PrT = transferDouble[8];

	double Fluxi[5] = { 0 };		//清零
	double UL[5], UR[5], UL2[5], UR2[5], QL[5], QR[5], Flux0[5];
	double U0[5][5];
	//$OMP ENDDO
	//---- - i - direction----------------------------------------------------------------------------------
	if (i >= 1 && i <= nx && j >= 1 && j <= ny - 1) {
			int flagL = (i + LAP)*(nn+1) + j + 1 + LAP;
			int flagR= (i + LAP)*(nn+1) + j + LAP;
			double dx = x[flagL] - x[flagR];
			double dy = y[flagL] - y[flagR];
			const double si = sqrt(dx*dx + dy*dy);	//边长
			const double ni1 = dy / si;
			const double ni2 = -dx / si;   //normal vector at(i, j) or (I - 1 / 2, J)
			int flag1 = (i + LAP)*(nn+1) + j+LAP;	int flag2 = (i + 1 + LAP)*(nn+1) + j + LAP;
			const double vol= abs((x[flag1] - x[flag2+1])*(y[flag2] - y[flag1+1]) -
				(x[flag2] - x[flag1+1])*(y[flag1] - y[flag2+1]))*0.5e0;

			for (int k = 1; k <= 4; ++k) {
				for (int m = 1; m <= 4; ++m) {
					int flag = (i - 3 + m + LAP)*(nn1 + 1) * 5 + (j + LAP) * 5 + k;
					U0[k][m] = U[flag];
				
				}
			}
			cuda_Reconstuction_Characteristic(U0, UL, UR, gamma);

			//------ - 坐标旋转到垂直于界面 （法向 - 切向 坐标系）
			QL[1] = UL[1]; QL[2] = UL[2] * ni1 + UL[3] * ni2; QL[3] = -UL[2] * ni2 + UL[3] * ni1; QL[4] = UL[4]; //密度、压力、法向速度、切向速度 （左值）
			QR[1] = UR[1]; QR[2] = UR[2] * ni1 + UR[3] * ni2; QR[3] = -UR[2] * ni2 + UR[3] * ni1; QR[4] = UR[4]; //密度、压力、法向速度、切向速度 （右值）

			cuda_Flux_Van_Leer_1Da(QL, QR, Flux0, gamma);

			//------------------------------------------------
			//算出通量 （变换回x - y坐标系）
			Fluxi[1] = -Flux0[1] * si;                            //质量通量
			Fluxi[2] = -(Flux0[2] * ni1 - Flux0[3] * ni2)*si;         //x - 方向动量通量
			Fluxi[3] = -(Flux0[2] * ni2 + Flux0[3] * ni1)*si;         //y - 方向动量通量
			Fluxi[4] = -Flux0[4] * si;                            //能量通量

		

			 //--i - 方向无粘通量计算结束，计算i - 方向粘性通量
			 //---------------------------------------------------------------------------------------------------------
			 //---------- - Viscous term--------------------------------------------------------------------------------
			 //扩散系数（粘性系数、热传导系数） = 界面两侧值的平均 （边界处采用单侧值)
			double Amu1 = 0; double Amk1 = 0;
			const int colum = nn1 + 1;
			if (i == 1) {
				const int flag = (i + LAP)*colum + j + LAP;
				Amu1 = Amu[flag] + Amu_t[flag];                  //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*(Amu[flag] / Pr + Amu_t[flag] / PrT);     //热传导系数
			}
			else if (i == nx) {
				const int flag = (i - 1 + LAP)*colum + j + LAP;
				Amu1 = Amu[flag] + Amu_t[flag];                  //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*(Amu[flag] / Pr + Amu_t[flag] / PrT);     //热传导系数
			}
			else {
				const int flag1 = (i - 1 + LAP)*colum + j + LAP;
				const int flag2 = (i + LAP)*colum + j + LAP;
				Amu1 = (Amu[flag1] + Amu[flag2] + Amu_t[flag1] + Amu_t[flag2])*0.5E0;                 //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*((Amu[flag1] + Amu[flag2]) / Pr + (Amu_t[flag1] + Amu_t[flag2]) / PrT)*0.5e0;   //热传导系数
			}

		/*	if (i == 1 && j == 1) {
				printf("Amu= %e, Amk= %e \n", Amu1, Amk1);
				printf("Cp= %e, Pr=%e,  PrT=%e \n", Cp, Pr, PrT);
				printf("Amu= %e, Amk= %e \n", Amu[flag], Amu_t[flag]);
			}*/

			//----Jocabian系数 （物理坐标对计算坐标的导数, 用于计算物理量的导数）
			int myi = i + LAP;	int myj = j + LAP;
			flag1 = myi*colum + myj;
			flag2 = (myi - 1)*colum + myj;
			const int flag3 = (myi - 1)*colum + myj + 1;
			const int flag4 = (myi - 1)*colum + myj - 1;
			double Dix =  x1[flag1] -  x1[flag2];
			double Diy =  y1[flag1] -  y1[flag2];
			double Djx = ( x1[flag3] +  x1[flag1 + 1] -  x1[flag4] -  x1[flag4])*0.25E0;
			double Djy = ( y1[flag3] +  y1[flag1 + 1] -  y1[flag4] -  y1[flag4])*0.25E0;
			double Ds = 1.E0 / (Dix*Djy - Djx*Diy);
			//物理量对计算坐标的导数
			double Diu =  uu[flag1] -  uu[flag2];
			double Div =  vv[flag1] -  vv[flag2];
			double DiT =  T[flag1] -  T[flag2];
			double Dju = ( uu[flag3] +  uu[flag1 + 1] -  uu[flag4] -  uu[flag4])*0.25E0;
			double Djv = ( vv[flag3] +  vv[flag1 + 1] -  vv[flag4] -  vv[flag4])*0.25E0;
			double DjT = ( T[flag3] +  T[flag1 + 1] -  T[flag4] -  T[flag4])*0.25E0;
			//物理量对x, y坐标的导数
			double ux = (Diu*Djy - Dju*Diy)*Ds;
			double vx = (Div*Djy - Djv*Diy)*Ds;
			double Tx = (DiT*Djy - DjT*Diy)*Ds;
			double uy = (-Diu*Djx + Dju*Dix)*Ds;
			double vy = (-Div*Djx + Djv*Dix)*Ds;
			double Ty = (-DiT*Djx + DjT*Dix)*Ds;
			//粘性应力及能量通量
			double u1 = ( uu[flag1] +  uu[flag2])*0.5E0;
			double v1 = ( vv[flag1] +  vv[flag2])*0.5E0;
			double t11 = ((4.E0 / 3.E0)*ux - (2.E0 / 3.E0)*vy)*Amu1;
			double t22 = ((4.E0 / 3.E0)*vy - (2.E0 / 3.E0)*ux)*Amu1;
			double t12 = (uy + vx)*Amu1;
			double E1 = u1*t11 + v1*t12 + Amk1*Tx;
			double E2 = u1*t12 + v1*t22 + Amk1*Ty;
			//添加粘性通量
			Fluxi[2] += (t11*ni1 + t12*ni2)*si;
			Fluxi[3] += (t12*ni1 + t22*ni2)*si;
			Fluxi[4] += (E1*ni1 + E2*ni2)*si;

			int loc = i*(ny+1) * 5 + j * 5;
			for (int m = 1; m <= 4; ++m) {
				tFluxi[loc + m] = Fluxi[m];
			}

			//if (i == 1 && j == 1) {
			//	printf("\n debug i = %d, %d, %e , %e, %e, %e\n",i,j, Fluxi[1], Fluxi[2], Fluxi[3], Fluxi[4]);
			//	//printf("%10.9e, %10.9e, %10.9e, \n %10.9e, %10.9e, %10.9e, %10.9e, %10.9e\n ", si, ni1, ni2, t11, t12, t22, E1, E2);
			//	printf("%10.9e, %10.9e, %10.9e\n\n", put1, put2, put3);
			//}
			//if (i == 2 && j == 1) {
			//	printf("%d, %d, %e , %e, %e, %e\n", i, j, Fluxi[1], Fluxi[2], Fluxi[3], Fluxi[4]);
			//	printf("%10.9e, %10.9e, %10.9e\n\n", put1, put2, put3);
			//}
			//if (i == 1 && j == 2) {
			//	printf("%d, %d, %e , %e, %e, %e\n", i, j, Fluxi[1], Fluxi[2], Fluxi[3], Fluxi[4]);
			//	printf("%10.9e, %10.9e, %10.9e\n\n", put1, put2, put3);
			//}
	}
}


//计算 j 方向的数值通量
__global__
void getResidual_j_WithKernel(int * transferInt, double *transferDouble, double* U, double * Amu, double *Amu_t,
	double * x1, double *y1, double *x, double *y, double *tFluxj, double *uu, double *vv, double * T)
{
	__shared__  int  nx, ny, LAP, mm, nn, mm1, nn1, int colum;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];	ny = transferInt[1];
	LAP = transferInt[2];
	mm = nx + 2 * LAP; nn = ny + 2 * LAP;
	mm1 = nx + 2 * LAP - 1; nn1 = ny + 2 * LAP - 1;
	colum = nn1 + 1;
	__shared__ double p00, gamma, Cp, Pr, PrT;
	p00 = transferDouble[0];
	gamma = transferDouble[1];
	Cp = transferDouble[6];
	Pr = transferDouble[7];
	PrT = transferDouble[8];

	double Fluxj[5] = { 0 };		//清零
	double UL[5], UR[5], UL2[5], UR2[5], QL[5], QR[5], Flux0[5];
	double U0[5][5];

	//= == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == =
	//j - 方向的无粘及粘性通量
	//---------------------------------------- - j - direction------------------------------------------------------------ -
	//$OMP DO
	if (i >= 1 && i <= nx - 1) {
		if (j >= 1 && j <= ny) {
			//边长，法方向
			int flagL = (i + 1 + LAP)*(nn + 1) + j + LAP;
			int flagR = (i + LAP)*(nn + 1) + j + LAP;
			double dx = x[flagL] - x[flagR];
			double dy = y[flagL] - y[flagR];
			const double sj = sqrt(dx*dx + dy*dy);	//边长
			const double nj1 = -dy / sj;
			const double nj2 = dx / sj;   //normal vector at(i, j) or (I - 1 / 2, J)

			//左右守恒变量的重构
			for (int k = 1; k <= 4; ++k) {
				for (int m = 1; m <= 4; ++m) {
					U0[k][m] = U[(i + LAP)*colum*5+(j - 3 + m + LAP)*5+k];
				}
			}
			cuda_Reconstuction_Characteristic(U0, UL, UR, gamma);
			
			//------ - 坐标旋转到垂直于界面 （法向 - 切向 坐标系）
			QL[1] = UL[1]; QL[2] = UL[2] * nj1 + UL[3] * nj2; QL[3] = -UL[2] * nj2 + UL[3] * nj1; QL[4] = UL[4]; //密度、压力、法向速度、切向速度 （左值）
			QR[1] = UR[1]; QR[2] = UR[2] * nj1 + UR[3] * nj2; QR[3] = -UR[2] * nj2 + UR[3] * nj1; QR[4] = UR[4]; //密度、压力、法向速度、切向速度 （右值）

			cuda_Flux_Van_Leer_1Da(QL, QR, Flux0, gamma);

			//------------------------------------------------
			//算出通量 （变换回x - y坐标系）
			Fluxj[1] = -Flux0[1] * sj;                            //质量通量
			Fluxj[2] = -(Flux0[2] * nj1 - Flux0[3] * nj2)*sj;         //x - 方向动量通量
			Fluxj[3] = -(Flux0[2] * nj2 + Flux0[3] * nj1)*sj;         //y - 方向动量通量
			Fluxj[4] = -Flux0[4] * sj;                            //能量通量

			double Amu1 = 0;	double Amk1 = 0;
			if (j == 1) {
				int flag = (i + LAP)*colum + j + LAP;
				Amu1 = Amu[flag] + Amu_t[flag];
				Amk1 = Cp*(Amu[flag] / Pr + Amu_t[flag] / PrT);   //热传导系数
			}
			else if (j == ny) {
				int flag = (i + LAP)*colum + j - 1 + LAP;
				Amu1 = Amu[flag] + Amu_t[flag];
				Amk1 = Cp*(Amu[flag] / Pr + Amu_t[flag] / PrT);   //热传导系数
			}
			else {
				int flag1 = (i + LAP)*colum + j + LAP;
				int flag2 = (i + LAP)*colum + j - 1 + LAP;
				Amu1 = (Amu[flag1] + Amu[flag2] + Amu_t[flag1] + Amu_t[flag2])*0.5E0;
				Amk1 = Cp*((Amu[flag2] + Amu[flag1]) / Pr + (Amu_t[flag2] + Amu_t[flag1]) / PrT)*0.5E0;   //热传导系数
			}

			int myi = i + LAP;	int myj = j + LAP;
			const int flag1 = (myi + 1)*colum + myj - 1;
			const int flag2 = (myi - 1)*colum + myj;
			const int flag3 = (myi - 1)*colum + myj - 1;
			const int flag4 = (myi + 1)*colum + myj;
			const int flag5 = myi*colum + myj;
			double Dix = ( x1[flag1] +  x1[flag4] -  x1[flag3] -  x1[flag2])*0.25E0;
			double Diy = ( y1[flag1] +  y1[flag4] -  y1[flag3] -  y1[flag2])*0.25E0;
			double Djx =  x1[flag5] -  x1[flag5 - 1];
			double Djy =  y1[flag5] -  y1[flag5 - 1];

			double Ds = 1.E0 / (Dix*Djy - Djx*Diy);

			double Diu = ( uu[flag1] +  uu[flag4] -  uu[flag3] -  uu[flag2])*0.25E0;
			double Div = ( vv[flag1] +  vv[flag4] -  vv[flag3] -  vv[flag2])*0.25E0;
			double DiT = ( T[flag1] +  T[flag4] -  T[flag3] -  T[flag2])*0.25E0;
			double Dju =  uu[flag5] -  uu[flag5 - 1];
			double Djv =  vv[flag5] -  vv[flag5 - 1];
			double DjT =  T[flag5] -  T[flag5 - 1];
			//
			double ux = (Diu*Djy - Dju*Diy)*Ds;
			double vx = (Div*Djy - Djv*Diy)*Ds;
			double Tx = (DiT*Djy - DjT*Diy)*Ds;
			double uy = (-Diu*Djx + Dju*Dix)*Ds;
			double vy = (-Div*Djx + Djv*Dix)*Ds;
			double Ty = (-DiT*Djx + DjT*Dix)*Ds;
			double t11 = ((4.E0 / 3.E0)*ux - (2.E0 / 3.E0)*vy)*Amu1;
			double t22 = ((4.E0 / 3.E0)*vy - (2.E0 / 3.E0)*ux)*Amu1;
			double t12 = (uy + vx)*Amu1;
			double	u1 = ( uu[flag5] +  uu[flag5 - 1])*0.5E0;
			double v1 = ( vv[flag5] +  vv[flag5 - 1])*0.5E0;

			double E1 = u1*t11 + v1*t12 + Amk1*Tx;
			double E2 = u1*t12 + v1*t22 + Amk1*Ty;

			Fluxj[2] += (t11*nj1 + t12*nj2)*sj;
			Fluxj[3] += (t12*nj1 + t22*nj2)*sj;
			Fluxj[4] += (E1*nj1 + E2*nj2)*sj;

	/*		if (i == 1 && j == 1) {
				printf(" debugj= %d, %d, %12.10e , %e, %e, %e\n", i, j, Fluxj[1], Fluxj[2], Fluxj[3], Fluxj[4]);
			}
			if (i == 16 && j == 29) {
				printf("%d, %d, %e , %e, %e, %e\n", i, j, Fluxj[1], Fluxj[2], Fluxj[3], Fluxj[4]);
			}
			if (i == 36 && j == 13) {
				printf(" %d, %d, %e , %e, %e, %e\n", i, j, Fluxj[1], Fluxj[2], Fluxj[3], Fluxj[4]);
			}*/

			int loc = i*(ny + 1) * 5 + j* 5;

			for (int m = 1; m <= 4; ++m) {
				tFluxj[loc + m] =Fluxj[m];
			}

			//printf("*&= %d,  %d, %10.9f, %10.9f, %10.9f, %10.9f\n", i, j, Fluxj[1], Fluxj[2], Fluxj[3], Fluxj[4]);

			//if (i == 14 && j == 6) {
			//	printf("*&= %d,  %d, %10.9f,   %f, %f, %f\n", nn1, mm1, Fluxj[1], Fluxj[2], Fluxj[3], Fluxj[4]);
			//	printf("Amu= %d,  %d,  %f, %f,  %f, %f \n", LAP, j, Res[locL + 1], Res[locL + 2], Res[locL + 3], Res[locL + 4]);
			//	printf("Amu= %d,  %d,  %f, %f,  %f, %f \n", LAP, j, Res[locR + 1], Res[locR + 2], Res[locR + 3], Res[locR + 4]);
			//	printf("Amu= %f,  %f,  %f, %f \n", U0[1][1], U0[2][2], U0[3][1], U0[4][3]);
			//	printf("Amu= %d,  %d,  %f, %f \n", i, j, nj1, nj2);
			//}

		}
	}

}


//----------------------------------------------------------
//利用守恒变量，计算基本量(d, u, v, T, p, c)
//----------------------------------------------------------
__global__
void comput_duvtpckw_with_cuda(int * transferInt, double *transferDouble, double *d,
			double * uu,double * vv, double *T,   double * U)
{
	__shared__  int nx, ny, LAP, mm1, nn1;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];
	ny = transferInt[1];
	LAP = transferInt[2];
	__shared__ double p00, Cv, Ma;
	p00 = transferDouble[0];
	Cv = transferDouble[2];
	Ma = transferDouble[3];
	mm1 = nx + 2 * LAP - 1;
	nn1 = ny + 2 * LAP - 1;
	if (i >= 1 && i <= mm1) {
		if (j >= 1 && j <= nn1) {
			int flag = i*(nn1+1) + j;
			int flag2= i*(nn1 + 1)*5 + j*5;
			d[flag] = U[flag2 + 1];
			uu[flag] = U[flag2+2] / d[flag];
			vv[flag] = U[flag2+3] / d[flag];
			T[flag] = (U[flag2+4] - 0.5e0*d[flag] * (uu[flag] * uu[flag] + vv[flag] * vv[flag])) / (Cv*d[flag]);
			//if (i == 4 && j == 6) { printf("%d,  %d,  %f\n", i, j, d); }
		}
	}
}


//计算层流粘性系数
__global__
void get_viscous_with_cuda(int * transferInt, double *transferDouble, double *Amu, double * T)
{
	__shared__  int nx, ny, LAP, nxM, nyM, ny1;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];
	ny = transferInt[1];
	LAP = transferInt[2];
	__shared__ double T_inf, Tsb, Re;
	T_inf = transferDouble[4];
	Tsb = 110.4E0 / T_inf;
	Re = transferDouble[5];

	nxM = nx + LAP;	nyM = ny + LAP;	ny1 = ny + 2 * LAP - 1;
	if (i >= LAP&&i <= nxM) {
		if (j >= LAP&&j <= nyM) {
			int flag = i*(ny1+1) + j;
			Amu[flag] = 1.E0 / Re*(1.E0 + Tsb)*sqrt(T[flag] * T[flag] * T[flag]) / (Tsb + T[flag]);
		}
	}
}


//利用特征变量重构
__device__
void cuda_Reconstuction_Characteristic(double U0[5][5], double *UL, double * UR, double gamma)
{
	//real * 8::Uh(4), S(4, 4), S1(4, 4), V0(4, 4), VL(4), VR(4), QL(4), QR(4)
	//real * 8::V2, d1, u1, v1, p1, c1, tmp0, tmp1, tmp3, tmp5

	//U0(m, k) : k = 1, 4 for  i - 2, i - 1, i, i + 1; m for the conservative variables U0(1, m) = d, U0(2, m) = d*u, ....
	double Uh[5] = { 0 };
	for (int i = 1; i <= 4; ++i) {
		Uh[i] = 0.5E0*(U0[i][2] + U0[i][3]);	  //conservative variables in the point I - 1 / 2  (or i)
	}
	double d1 = Uh[1]; double u1 = Uh[2] / d1; double v1 = Uh[3] / d1;
	double p1 = (Uh[4] - (Uh[2] * u1 + Uh[3] * v1)*0.5E0)*(gamma - 1.E0);  //density, velocity, pressure and sound speed
	double c1 = sqrt(gamma*p1 / d1);

	double V2 = (u1*u1 + v1*v1)*0.5E0;
	double tmp1 = (gamma - 1.E0) / c1;
	double tmp3 = (gamma - 1.E0) / (c1*c1);
	double tmp5 = 1.E0 / (2.E0*c1);
	double tmp0 = 1.E0 / tmp3;

	//A = S(-1)*LAMDA*S    see 《计算空气动力学》 158 - 159页(with alfa = 1, beta = 0)
	double S[5][5];		double S1[5][5];

	S[1][1] = V2 - tmp0;       S[1][2] = -u1;         S[1][3] = -v1;      S[1][4] = 1.E0;
	S[2][1] = -v1;          S[2][2] = 0.E0;        S[2][3] = 1.E0;     S[2][4] = 0.E0;
	S[3][1] = -u1 - V2*tmp1;   S[3][2] = 1.E0 + tmp1*u1; S[3][3] = tmp1*v1;   S[3][4] = -tmp1;
	S[4][1] = -u1 + V2*tmp1;   S[4][2] = 1.E0 - tmp1*u1; S[4][3] = -tmp1*v1;  S[4][4] = tmp1;

	S1[1][1] = -tmp3;    S1[1][2] = 0.E0;   S1[1][3] = -tmp5;         S1[1][4] = tmp5;
	S1[2][1] = -tmp3*u1; S1[2][2] = 0.E0;   S1[2][3] = 0.5E0 - u1*tmp5; S1[2][4] = 0.5E0 + u1*tmp5;
	S1[3][1] = -tmp3*v1; S1[3][2] = 1.E0;   S1[3][3] = -v1*tmp5;       S1[3][4] = v1*tmp5;
	S1[4][1] = -tmp3*V2; S1[4][2] = v1;     S1[4][3] = (c1*u1 - V2 - tmp0)* tmp5; S1[4][4] = (c1*u1 + V2 + tmp0) * tmp5;

	//V = SU      V[k] = S*U[k]
	double V0[5][5];
	for (int k = 1; k <= 4; ++k) {
		for (int m = 1; m <= 4; ++m) {
			V0[k][m] = 0.E0;
			for (int j = 1; j <= 4; ++j) {
				V0[k][m] = V0[k][m] + S[m][j] * U0[j][k];
			}
		}
	}
	double VL[5] = { 0 }; double VR[5] = { 0 };
	for (int m = 1; m <= 4; ++m) {
		cuda_scheme_fP(VL[m], V0[1][m], V0[2][m], V0[3][m], V0[4][m]);
		cuda_scheme_fm(VR[m], V0[1][m], V0[2][m], V0[3][m], V0[4][m]);
	}

	double QL[5] = { 0 }; double QR[5] = { 0 };
	for (int m = 1; m <= 4; ++m) {
		QL[m] = 0.E0; QR[m] = 0.E0;
		for (int j = 1; j <= 4; ++j) {
			QL[m] = QL[m] + S1[m][j] * VL[j];
			QR[m] = QR[m] + S1[m][j] * VR[j];
		}
	}

	UL[1] = QL[1]; UL[2] = QL[2] / UL[1]; UL[3] = QL[3] / UL[1];
	UL[4] = (QL[4] - (UL[2] * QL[2] + UL[3] * QL[3])*0.5E0)* (gamma - 1.E0);  //density, velocity, pressure and sound speed
	UR[1] = QR[1]; UR[2] = QR[2] / UR[1]; UR[3] = QR[3] / UR[1];
	UR[4] = (QR[4] - (UR[2] * QR[2] + UR[3] * QR[3]) * 0.5E0) * (gamma - 1.E0);  //find a bug, removed
}


//---------------------------------------------------------- -
//数值格式，构造UL = U(j + 1 / 2, L); u1 = u(j - 1), u2 = u(j), u3 = u(j + 1), u4 = u(j + 2)
__device__
void cuda_scheme_fP(double &UL, double u1, double u2, double u3, double u4)
{
	const double k = 1.E0;
	const double k3 = 1.E0 / 3.E0, ep = 1.E-6;

	//Iflag_Scheme == Scheme_MUSCL3         //3阶MUSCL(Van Albada限制器)
	double up = u3 - u2; double um = u2 - u1;//1阶 前差、后差
	double s = (2.E0*up*um + ep) / (up*up + um*um + ep);//Van Albada限制器 （光滑区，前差与后差接近，该值接近1）
	UL = u2 + 0.25E0*s*((1.E0 - k3*s)*um + (1.E0 + k3*s)*up);//3阶MUSCL(光滑区逼近3阶迎风)
}

//数值格式，构造UR = U(j + 1 / 2, R); u1 = u(j - 1), u2 = u(j), u3 = u(j + 1), u4 = u(j + 2)
__device__
void cuda_scheme_fm(double & UR, double u1, double u2, double u3, double u4)
{
	const double k = 1.E0 / 3.0;
	const double k3 = 1.E0 / 3.E0, ep = 1.E-6;
	
	//Iflag_Scheme == Scheme_MUSCL3      //3阶MUSCL(Van Albada限制器)
	double up = u4 - u3; double um = u3 - u2;                             //前差、后差
	double s = (2.E0*up*um + ep) / (up*up + um*um + ep);
	UR = u3 - 0.25E0*s*((1.E0 - k3*s)*up + (1.E0 + k3*s)*um);
}


//---------------------------------------------------------- -
//Code by Cofludy according to Leng Yan`s code 
__device__
void  cuda_Flux_Van_Leer_1Da(double *QL, double * QR, double *Flux, const double gamma)
{
	double dl, uul, vvl, pl, al, dr, uur, vvr, pr, ar, Ml, Mr, Mp, Mm;  //uu velocity
	double tmp0, fp[5], fm[5];

	dl = QL[1]; uul = QL[2]; vvl = QL[3];  pl = QL[4];
	dr = QR[1]; uur = QR[2]; vvr = QR[3];  pr = QR[4];
	al = sqrt(gamma*pl / dl);  //density, velocity, pressure and sound speed
	ar = sqrt(gamma*pr / dr);
	Ml = uul / (al); Mr = uur / (ar);
	if (Ml >= 1.E0) {
		fp[1] = dl*uul;
		fp[2] = dl*uul*uul + pl;
		fp[3] = dl*uul*vvl;
		fp[4] = uul*(gamma*pl / (gamma - 1.E0) + 0.5E0*dl*(uul*uul + vvl*vvl));
	}
	else if (abs(Ml)<1.E0) {
		Mp = 0.25E0*(1.E0 + Ml)*(1.E0 + Ml);
		tmp0 = dl*al*Mp;
		fp[1] = tmp0;
		fp[2] = tmp0*((gamma - 1.E0)*uul + 2.E0*al) / gamma;
		fp[3] = tmp0*vvl;
		fp[4] = tmp0*(((gamma - 1.E0)*uul + 2.E0*al)*((gamma - 1.E0)*uul + 2.E0*al)*0.5E0 / (gamma*gamma - 1.E0) + 0.5E0*(vvl*vvl));
	}
	else if (Ml <= -1.E0) {
		fp[1] = 0.E0;
		fp[2] = 0.E0;
		fp[3] = 0.E0;
		fp[4] = 0.E0;
	}

	if (Mr >= 1.E0) {
		fm[1] = 0.E0;
		fm[2] = 0.E0;
		fm[3] = 0.E0;
		fm[4] = 0.E0;
	}
	else if (abs(Mr) < 1.E0) {
		Mm = -0.25E0*(Mr - 1.E0) * (Mr - 1.E0);
		tmp0 = dr*ar*Mm;
		fm[1] = tmp0;
		fm[2] = tmp0*((gamma - 1.E0) * uur - 2.E0*ar) / gamma;
		fm[3] = tmp0*vvr;
		fm[4] = tmp0*(((gamma - 1.E0)*uur - 2.E0*ar)*((gamma - 1.E0)*uur - 2.E0*ar)*0.5E0 / (gamma*gamma - 1.E0) + 0.5E0*(vvr*vvr));
	}
	else if (Mr <= -1.E0) {
		fm[1] = dr*uur;
		fm[2] = dr*uur*uur + pr;
		fm[3] = dr*uur*vvr;
		fm[4] = uur*(gamma*pr / (gamma - 1.E0) + 0.5E0*dr*(uur*uur + vvr*vvr));
	}
	for (int i = 1; i <= 4; ++i) {
		Flux[i] = fp[i] + fm[i];
	}

}