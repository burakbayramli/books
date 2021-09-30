#include"turbulence_SST.cuh"
#include "common.h"
#include "Global_var.h"
#include "Flow_var.h"
#include "sub_turbulence_SST.h"
#include "cmath"

__device__
double cuda_min(double a, double b) {
	return a < b ? a : b;
}

__device__
double cuda_max(double a, double b) {
	return a > b ? a : b;
}


void Amut_boundary(int mBlock);

__global__
void cuda_calcu_Qt(int *transferInt, double * transferDouble, double * x1, double *y1, double * d, double * uu, double * vv, double *Kt, double * Wt,
	double *dw, double *x, double * y, double *f1, double *Amu, double *Amu_t, double * Res5, double * Res6);

__global__
void cuda_calcu_i_devdiff(int *transferInt, double * transferDouble, double * x, double * y, double *uu, double * vv,
	double *x1, double * y1, double * Kt, double * Wt, double *f1, double * Amu, double * Amu_t, double *Res5, double * Res6);

__global__
void cuda_calcu_j_devdiff(int *transferInt, double * transferDouble, double * x, double * y, double *uu, double * vv,
	double *x1, double * y1, double * Kt, double * Wt, double *f1, double * Amu, double * Amu_t, double *Res5, double *Res6);


void turbulence_SST_kw_before_cuda(double * Amu_t_dev, double * Amu_dev, double *d, double *uu, double *vv, double * T,
	double *U_dev,double *x_dev, double *y_dev, double *x1_dev, double *y1_dev,int mBlock, flow_var & fl, int * transferInt_dev, double * transferDouble_dev)
{
	Block_TYPE & B = Mesh[1].Block[mBlock];
	const int nx =  B.nx;	const int ny =  B.ny;
	const int mm1 = nx + 2 * LAP - 1;	const int nn1 = ny + 2 * LAP - 1;
	const int mm = nx + 2 * LAP;	const int nn = ny + 2 * LAP;

	dim3 threadPerBlock(16, 16);
	dim3  blockPerGrid((nx + 2 * LAP + 1 + threadPerBlock.x - 1) / threadPerBlock.x, (ny + 2 * LAP + 1 + threadPerBlock.y - 1) / threadPerBlock.y);

	//将壁面距离信息拷贝到设备
	double * dw_dev;	double *dw_host;
	HANDLE_ERROR(cudaMallocHost((double **)& dw_host, (nx + 1)*(ny + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)&dw_dev, (nx + 1)*(ny + 1) * sizeof(double)));
	for (int i = 1; i <= nx; ++i) {
		for (int j = 1; j <= ny; ++j) {
			dw_host[i*(ny + 1) + j] =  B.dw[i][j];
		}
	}
	HANDLE_ERROR(cudaMemcpy(dw_dev, dw_host, (nx + 1)*(ny + 1) * sizeof(double), cudaMemcpyHostToDevice));
	HANDLE_ERROR(cudaFreeHost(dw_host));


	//对湍流粘性系数付初值
	HANDLE_ERROR(cudaMemset(Amu_t_dev, 0, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	
	/*return*/
	return;


	double * Kt; double *  Wt; double * f1;		//[nx+1][ny+1]
	//double * Qk; double * Qw;		// [mm1 + 1][nn1 + 1]
	
	HANDLE_ERROR(cudaMalloc((double **)& f1, (nx + 1)*(ny + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMemset(f1, 0, (nx + 1)*(ny + 1) * sizeof(double)));	//计算函数f1 （区分近壁区与远壁区）

	//HANDLE_ERROR(cudaMalloc((double **)& Qk, (nx + 1)*(ny + 1) * sizeof(double)));
	//HANDLE_ERROR(cudaMalloc((double **)& Qw, (nx + 1)*(ny + 1) * sizeof(double)));

	HANDLE_ERROR(cudaMalloc((double **)& Kt, (mm1 + 1)*(nn1 + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& Wt, (mm1 + 1)*(nn1 + 1) * sizeof(double)));

	double * Res5_dev;	double * Res6_dev;	//[nx+1][ny+1];
	HANDLE_ERROR(cudaMalloc((double **)& Res5_dev, (nx + 1)*(ny + 1) * sizeof(double)));
	HANDLE_ERROR(cudaMalloc((double **)& Res6_dev, (nx + 1)*(ny + 1) * sizeof(double)));

	//利用页锁定内存将剩余宏观量拷贝到设备
	double *Kt_host; double * Wt_host;
	HANDLE_ERROR(cudaHostAlloc((double **)&Kt_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaHostAllocDefault));
	HANDLE_ERROR(cudaHostAlloc((double **)&Wt_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaHostAllocDefault));
	for (int i = 1; i <= mm1; ++i) {
		for (int j = 1; j <= nn1; ++j) {
			int flag = i*(nn1 + 1) + j;
			Kt_host[flag] =  B.U[i][j][5] / fl.d[i][j];
			Wt_host[flag] =  B.U[i][j][6] / fl.d[i][j];
		}
	}
	HANDLE_ERROR(cudaMemcpy(Kt, Kt_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	HANDLE_ERROR(cudaMemcpy(Wt, Wt_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	HANDLE_ERROR(cudaFreeHost(Kt_host));
	HANDLE_ERROR(cudaFreeHost(Wt_host));

	//求源项
	cuda_calcu_Qt<<<blockPerGrid, threadPerBlock>>>(transferInt_dev, transferDouble_dev, x1_dev, y1_dev, d, uu, vv, Kt, Wt, dw_dev,
													x_dev, y_dev, f1, Amu_dev, Amu_t_dev,  Res5_dev, Res6_dev);

	HANDLE_ERROR(cudaThreadSynchronize());
	HANDLE_ERROR(cudaGetLastError());

	//求扩散项和对流项
	cuda_calcu_i_devdiff << <blockPerGrid, threadPerBlock >> >(transferInt_dev, transferDouble_dev, x_dev, y_dev, uu, vv,
		x1_dev, y1_dev, Kt, Wt, f1, Amu_dev, Amu_t_dev, Res5_dev, Res6_dev);

	cuda_calcu_j_devdiff << <blockPerGrid, threadPerBlock >> >(transferInt_dev, transferDouble_dev, x_dev, y_dev, uu, vv,
		x1_dev, y1_dev, Kt, Wt, f1, Amu_dev, Amu_t_dev, Res5_dev, Res6_dev);

	//将计算数据拷贝回主机端
	double *Res5_host;	double *Res6_host;
	HANDLE_ERROR(cudaHostAlloc((double **) & Res5_host, (nx + 1)*(ny + 1) * sizeof(double), cudaHostAllocDefault));
	HANDLE_ERROR(cudaHostAlloc((double **)& Res6_host, (nx + 1)*(ny + 1) * sizeof(double), cudaHostAllocDefault));
	HANDLE_ERROR(cudaMemcpy(Res5_host, Res5_dev, (nx + 1)*(ny + 1) * sizeof(double), cudaMemcpyDeviceToHost));
	HANDLE_ERROR(cudaMemcpy(Res6_host, Res6_dev, (nx + 1)*(ny + 1) * sizeof(double), cudaMemcpyDeviceToHost));

	double *Amu_host;	double *Amu_t_host;
	HANDLE_ERROR(cudaHostAlloc((double **)& Amu_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaHostAllocDefault));
	HANDLE_ERROR(cudaHostAlloc((double **)& Amu_t_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaHostAllocDefault));
	HANDLE_ERROR(cudaMemcpy(Amu_host, Amu_dev, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyDeviceToHost));
	HANDLE_ERROR(cudaMemcpy(Amu_t_host, Amu_t_dev, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyDeviceToHost));
	for (int i = 0; i < (mm1 + 1)*(nn1 + 1); ++i) {
		Amu_host[i] = Amu_host[i] / Re;
		Amu_t_host[i] = Amu_t_host[i] / Re;
	}
	HANDLE_ERROR(cudaMemcpy(Amu_dev, Amu_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));
	HANDLE_ERROR(cudaMemcpy(Amu_t_dev, Amu_t_host, (mm1 + 1)*(nn1 + 1) * sizeof(double), cudaMemcpyHostToDevice));

	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			int flag1 = i*(ny + 1) + j;
			B.Res[i][j][5] = Res5_host[flag1];
			B.Res[i][j][6] = Res6_host[flag1];
			int myi = i + LAP;	int myj = j + LAP;
			int flag2 = myi*(nn1 + 1) + myj;
			B.Amu[myi][myj] = Amu_host[flag2];
			B.Amu_t[myi][myj] = Amu_t_host[flag2];

			/*printf("%d, %d\n", i, j);
			printf("%e, %e, %e, %e\n", B.Res[i][j][5], B.Res[i][j][6], B.Amu[myi][myj], B.Amu_t[myi][myj]);
*/
			/*if (i == 3 && j == 14) {
				printf("T= %e, Amu= %e\n", T[flag], Amu[flag]);
			}*/
		}
	}
	//PAUSE;

	HANDLE_ERROR(cudaFreeHost(Res5_host));
	HANDLE_ERROR(cudaFreeHost(Res6_host));
	HANDLE_ERROR(cudaFreeHost(Amu_host));
	HANDLE_ERROR(cudaFreeHost(Amu_t_host));


	//设定湍流粘性系数虚网格的值
	Amut_boundary(mBlock);

	HANDLE_ERROR(cudaFree(f1));
	HANDLE_ERROR(cudaFree(dw_dev));
	//HANDLE_ERROR(cudaFree(Qk));	HANDLE_ERROR(cudaFree(Qw));
	HANDLE_ERROR(cudaFree(Kt)); HANDLE_ERROR(cudaFree(Wt));
	HANDLE_ERROR(cudaFree(Res5_dev)); HANDLE_ERROR(cudaFree(Res6_dev));
}


__global__
void cuda_calcu_Qt(int *transferInt, double * transferDouble, double * x1, double *y1,double * d, double * uu, double * vv,	double *Kt, double * Wt, 
	double *dw, double *x, double * y, double *f1, double *Amu, double *Amu_t,  double * Res5, double * Res6)
{
	__shared__  int nx, ny, LAP, nn1, nn;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];	ny = transferInt[1];
	LAP = transferInt[2];
	nn = ny + 2 * LAP;
	nn1 = ny + 2 * LAP-1;

	double Re = transferDouble[5];

	if (i >= 1 && i <= nx - 1 && j >= 1 && j <= ny - 1) {
		int myi = i + LAP;	int myj = j + LAP;

		//模型系数(1为k - w模型的系数，在近壁区使用； 2为k - epsl模型的系数在远壁区使用）
		//SST是一个k - w与k - epsl模型的混合模型，通过开关函数f来切换
		const double  beta1_SST = 0.075E0, Cw1_SST = 0.533E0;
		const double  sigma_w2_SST = 0.856E0, beta2_SST = 0.0828E0, Cw2_SST = 0.440E0;
		const double a1_SST = 0.31E0, betas_SST = 0.09E0;

		double Dix, Diy, Djx, Djy, Ds, Dik, Diw, Djk, Djw, Diu, Div, Dju, Djv, kx, ky, Wx, Wy, ux, vx, uy, vy;
		double  omega, Kws, CD_kw, arg1, arg2, arg3, f2, t11, t22, t12, Pk, Pk0;

		int flag = i*(ny + 1) + j;
		int flag1 = (myi + 1)*(nn1 + 1) + myj;
		int flag2 = (myi - 1)*(nn1 + 1) + myj;
		int flag3 = myi*(nn1 + 1) + myj;

		//计算物理量对坐标x, y的导数，使用Jocabian变换
		Dix = ( x1[flag1] -  x1[flag2])*0.5E0;
		Diy = ( y1[flag1] -  y1[flag2])*0.5E0;
		Djx = ( x1[flag3 + 1] -  x1[flag3 - 1])*0.5E0;
		Djy = ( y1[flag3 + 1] -  y1[flag3 - 1])*0.5E0;
		Ds = 1.E0 / (Dix*Djy - Djx*Diy);

		Diu = (uu[flag1] - uu[flag2])*0.5E0;
		Div = (vv[flag1] - vv[flag2])*0.5E0;
		Dik = (Kt[flag1] - Kt[flag2])*0.5E0;
		Diw = (Wt[flag1] - Wt[flag2])*0.5E0;

		Dju = (uu[flag3 + 1] - uu[flag3 - 1])*0.5E0;
		Djv = (vv[flag3 + 1] - vv[flag3 - 1])*0.5E0;
		Djk = (Kt[flag3 + 1] - Kt[flag3 - 1])*0.5E0;
		Djw = (Wt[flag3 + 1] - Wt[flag3 - 1])*0.5E0;

		//导数值
		ux = (Diu*Djy - Dju*Diy)*Ds;
		vx = (Div*Djy - Djv*Diy)*Ds;
		kx = (Dik*Djy - Djk*Diy)*Ds;
		Wx = (Diw*Djy - Djw*Diy)*Ds;

		uy = (-Diu*Djx + Dju*Dix)*Ds;
		vy = (-Div*Djx + Djv*Dix)*Ds;
		ky = (-Dik*Djx + Djk*Dix)*Ds;
		Wy = (-Diw*Djx + Djw*Dix)*Ds;

		//计算湍流粘性系数 ， Blazek's Book Eq. (7.66)    
		//if (i == 3 && j == 14) { printf("AMu= %e  ", Amu[flag3]); }
		Amu[flag3] = Amu[flag3] * Re;
		/*if (i == 3 && j == 14) {
			printf("   %e\n", Amu[flag3]);
		}*/

		omega = vx - uy;      //涡量
		arg2 = cuda_max(2.E0* sqrt(abs(Kt[flag3])) / (0.09*Wt[flag3] * dw[flag] * Re), 500.E0*Amu[flag3] / 
													(d[flag3] * Wt[flag3] * dw[flag] * dw[flag] * Re*Re));

		f2 = tanh(arg2*arg2);

		//////Revised by Wang XiangYu
		Amu_t[flag3] = cuda_min(cuda_min(d[flag3] * Kt[flag3] / Wt[flag3], a1_SST*d[flag3] * Kt[flag3] * Re / (f2*abs(omega))), 100000.);
		//计算f1(识别是否为近壁区，近壁区趋近于1）
		Kws = 2.E0*(kx*Wx + ky*Wy)*d[flag3] * sigma_w2_SST / (Wt[flag3] + 1.E-20);      //交叉输运项
		CD_kw = cuda_max(Kws, 1.E-20);
		arg3 = cuda_max(sqrt(abs(Kt[flag3])) / (0.09*Wt[flag3] * dw[flag] * Re), 500.E0*Amu[flag3] / (d[flag3] * Wt[flag3] * dw[flag] * dw[flag] * Re*Re));

		arg1 = cuda_min(arg3, 4.E0*d[flag3] * sigma_w2_SST*Kt[flag3] / (CD_kw*dw[flag] * dw[flag]));
		f1[flag] = tanh(arg1*arg1*arg1*arg1);             //开关函数，近壁区趋近于1，远壁区趋近于0  （用来切换k - w及k - epsl方程)

														  //湍应力 （使用了涡粘模型）
		t11 = ((4.E0 / 3.E0)*ux - (2.E0 / 3.E0)*vy)*Amu_t[flag3] - (2.E0 / 3.E0)*d[flag3] * Kt[flag3] * Re;   //Blazek's Book, Eq. (7.25)
		t22 = ((4.E0 / 3.E0)*vy - (2.E0 / 3.E0)*ux)*Amu_t[flag3] - (2.E0 / 3.E0)*d[flag3] * Kt[flag3] * Re;
		t12 = (uy + vx)*Amu_t[flag3];

		//湍能方程的源项（生成 - 耗散)
		//Pk = t11*ux + t22*vy + t12*(uy + vx)  //湍能生成项 （湍应力乘以应变率）
		Pk = Amu_t[flag3] * omega*omega;        //简化

		Pk0 = cuda_min(Pk, 20.E0*betas_SST*Kt[flag3] * Wt[flag3] * Re*Re);    //对湍能生成项进行限制，防止湍能过大

		double Qk = Pk0 / Re - betas_SST*d[flag3] * Wt[flag3] * Kt[flag3] * Re;    //k方程的源项  （生成项 - 耗散项）

		double Cw_SST = f1[flag] * Cw1_SST + (1.E0 - f1[flag])*Cw2_SST;    //模型系数，利用f1函数进行切换
		double beta_SST = f1[flag] * beta1_SST + (1.E0 - f1[flag])*beta2_SST;    //模型系数，利用f1函数进行切换
		//Qw[flag] = Cw_SST*d[flag]*Pk / ( Amu_t[flag] + 1.d - 20) / Re - beta_SST*d[flag]*Wt[flag]**2 * Re + (1.E0 - f1[flag])*Kws / Re     //W方程的源项
		double Qw = Cw_SST*d[flag3] * omega*omega / Re - beta_SST*d[flag3] * Wt[flag3] * Wt[flag3] * Re + (1.E0 - f1[flag])*Kws / Re;     //W方程的源项

		//计算该单元的面积 
		flag1 = (i + LAP)*(nn + 1) + j + LAP;	flag2 = (i + 1 + LAP)*(nn + 1) + j + LAP;
		const double vol = abs((x[flag1] - x[flag2 + 1])*(y[flag2] - y[flag1 + 1]) -
			(x[flag2] - x[flag1 + 1])*(y[flag1] - y[flag2 + 1]))*0.5e0;
		
		Res5[flag] += Qk * vol;
		Res6[flag] += Qw * vol;
	}
}


//计算对流项和扩散项
__global__
void cuda_calcu_i_devdiff(int *transferInt, double * transferDouble, double * x, double * y, double *uu, double * vv,
	double *x1, double * y1, double * Kt, double * Wt, double *f1, double * Amu, double * Amu_t, double *Res5, double * Res6)
{
	__shared__  int nx, ny, LAP, nn,  nn1;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];	ny = transferInt[1];
	LAP = transferInt[2];
	nn = ny + 2 * LAP;
	nn1 = ny + 2 * LAP - 1;
	double Re = transferDouble[5];

	if (i >= 1 && i <= nx && j >= 1 && j <= ny - 1) {

		const double sigma_k1_SST = 0.85E0, sigma_w1_SST = 0.5E0;
		const double sigma_k2_SST = 1.E0, sigma_w2_SST = 0.856E0;
		double sigma_K_SST, sigma_W_SST;
		double Dix, Diy, Djx, Djy, Ds, Dik, Diw, Djk, Djw, Diu, Div, Dju, Djv, kx, ky, Wx, Wy;

		int myi = i + LAP;	int myj = j + LAP;
		int flagL = (i + LAP)*(nn + 1) + j + 1 + LAP;
		int flagR = (i + LAP)*(nn + 1) + j + LAP;
		double dx = x[flagL] - x[flagR];
		double dy = y[flagL] - y[flagR];
		const double si = sqrt(dx*dx + dy*dy);	//边长
		const double ni1 = dy / si;
		const double ni2 = -dx / si;   //normal vector at(i, j) or (I - 1 / 2, J)

		int flag = i*(ny + 1) + j;
		int flag1 = myi*(nn1 + 1) + myj;
		int flag2 = (myi - 1)*(nn1 + 1) + myj;
									   //对流项，采用1阶迎风格式
		double un1 = uu[flag2] * ni1 + vv[flag2] * ni2;
		double un2 = uu[flag1] * ni1 + vv[flag1] * ni2;

		//1阶L - F 格式
		double Fluxk= -0.5E0*((un1 + abs(un1))*Kt[flag2] + (un2 - abs(un2))*Kt[flag1])*si;
		double Fluxw = -0.5E0*((un1 + abs(un1))*Wt[flag2] + (un2 - abs(un2))*Wt[flag1])*si;

		__syncthreads();

		//粘性项（扩散项），采用2阶中心格式

		//格式系数，k - w与k - epsl格式系数之间选择，(f1作为切换开关函数)
		sigma_K_SST = f1[flag] * sigma_k1_SST + (1.E0 - f1[flag])*sigma_k2_SST;
		sigma_W_SST = f1[flag] * sigma_w1_SST + (1.E0 - f1[flag])*sigma_w2_SST;

		//界面上的值 = 两侧值的平均, 边界上的扩散系数 = 内侧的值
		double Amu1, Amu2;
		if (i == 1) {
			Amu1 = Amu[flag1] + sigma_K_SST*Amu_t[flag1];         //扩散系数(k方程)
			Amu2 = Amu[flag1] + sigma_W_SST*Amu_t[flag1];         //扩散系数(w方程)
		}
		else if (i == nx) {
			Amu1 = Amu[flag2] + sigma_K_SST*Amu_t[flag2];        //扩散系数(k方程)
			Amu2 = Amu[flag2] + sigma_W_SST*Amu_t[flag2];        //扩散系数(w方程)
		}
		else {
			Amu1 = (Amu[flag2] + Amu[flag1] + sigma_K_SST*(Amu_t[flag2] + Amu_t[flag1]))*0.5E0;        //扩散系数(k方程), 界面上的值 = 两侧值的平均
			Amu2 = (Amu[flag2] + Amu[flag1] + sigma_W_SST*(Amu_t[flag2] + Amu_t[flag1]))*0.5E0;        //扩散系数(w方程)
		}

		//计算物理量（k, w）对坐标x, y的导数 （采用Jocabian变换）
		//----Jocabian系数 （物理坐标对计算坐标的导数, 用于计算物理量的导数）
		Dix = x1[flag1] - x1[flag2];
		Diy = y1[flag1] - y1[flag2];
		Djx = (x1[flag2 + 1] + x1[flag1 + 1] - x1[flag2 - 1] - x1[flag1 - 1])*0.25E0;
		Djy = (y1[flag2 + 1] + y1[flag1 + 1] - y1[flag2 - 1] - y1[flag1 - 1])*0.25E0;
		Ds = 1.E0 / (Dix*Djy - Djx*Diy);
		//物理量对计算坐标的导数
		Dik = Kt[flag1] - Kt[flag2];
		Diw = Wt[flag1] - Wt[flag2];
		Djk = (Kt[flag2 + 1] + Kt[flag1 + 1] - Kt[flag2 - 1] - Kt[flag1 - 1])*0.25E0;
		Djw = (Wt[flag2 + 1] + Wt[flag1 + 1] - Wt[flag2 - 1] - Wt[flag1 - 1])*0.25E0;
		//物理量对x, y坐标的导数
		kx = (Dik*Djy - Djk*Diy)*Ds;
		Wx = (Diw*Djy - Djw*Diy)*Ds;
		ky = (-Dik*Djx + Djk*Dix)*Ds;
		Wy = (-Diw*Djx + Djw*Dix)*Ds;
		//粘性应力及能量通量
		Fluxk += Amu1*(kx*ni1 + ky*ni2)*si / Re;
		Fluxw += Amu2*(Wx*ni1 + Wy*ni2)*si / Re;

		Res5[flag] -= Fluxk;
		Res6[flag] -= Fluxw;

		__syncthreads();
		flag = (i - 1)*(ny + 1) + j;
		Res5[flag] += Fluxk;
		Res6[flag] += Fluxw;
	}
}


//计算对流项和扩散项
__global__
void cuda_calcu_j_devdiff(int *transferInt, double * transferDouble, double * x, double * y, double *uu, double * vv,
	double *x1, double * y1, double * Kt, double * Wt, double *f1, double * Amu, double * Amu_t, double *Res5, double *Res6)
{
	__shared__  int nx, ny, LAP, nn, nn1;
	int i = blockDim.x*blockIdx.x + threadIdx.x;
	int j = blockDim.y*blockIdx.y + threadIdx.y;
	nx = transferInt[0];	ny = transferInt[1];
	LAP = transferInt[2];
	nn = ny + 2 * LAP;
	nn1 = ny + 2 * LAP - 1;
	double Re = transferDouble[5];

	if (i >= 1 && i <= nx-1 && j >= 1 && j <= ny) {

		const double sigma_k1_SST = 0.85E0, sigma_w1_SST = 0.5E0;
		const double sigma_k2_SST = 1.E0, sigma_w2_SST = 0.856E0;
		double sigma_K_SST, sigma_W_SST;
		double Dix, Diy, Djx, Djy, Ds, Dik, Diw, Djk, Djw, Diu, Div, Dju, Djv, kx, ky, Wx, Wy;
	
		//边长，法方向
		int flagL = (i + 1 + LAP)*(nn + 1) + j + LAP;
		int flagR = (i + LAP)*(nn + 1) + j + LAP;
		double dx = x[flagL] - x[flagR];
		double dy = y[flagL] - y[flagR];
		const double sj = sqrt(dx*dx + dy*dy);	//边长
		const double nj1 = -dy / sj;
		const double nj2 = dx / sj;   //normal vector at(i, j) or (I - 1 / 2, J)

									  //对流项，采用1阶迎风格式 （L - F分裂）
		int myi = i + LAP;	int myj = j + LAP;
		int flag = i*(ny + 1) + j;
		int flag1 = myi*(nn1 + 1) + myj;
		int flag2 = (myi + 1)*(nn1 + 1) + myj;
		int flag3 = (myi - 1)*(nn1 + 1) + myj;

		double un1 =  uu[flag1 - 1] * nj1 + vv[flag1 - 1] * nj2;
		double un2 =  uu[flag1] * nj1 + vv[flag1] * nj2;
		double Fluxk= -0.5E0*((un1 + abs(un1))*Kt[flag1 - 1] + (un2 - abs(un2))*Kt[flag1])*sj;
		double Fluxw= -0.5E0*((un1 + abs(un1))*Wt[flag1 - 1] + (un2 - abs(un2))*Wt[flag1])*sj;

		__syncthreads();
		//粘性项
		//-------- - Vmyiscous term---------------------------------------------------------------------------- -
		sigma_K_SST = f1[flag] * sigma_k1_SST + (1.E0 - f1[flag])*sigma_k2_SST;
		sigma_W_SST = f1[flag] * sigma_w1_SST + (1.E0 - f1[flag])*sigma_w2_SST;
		double Amu1, Amu2;
		if (j == 1) {
			Amu1 =  Amu[flag1] + sigma_K_SST* Amu_t[flag1];
			Amu2 =  Amu[flag1] + sigma_W_SST* Amu_t[flag1];
		}
		else if (j == ny) {
			Amu1 =  Amu[flag1 - 1] + sigma_K_SST* Amu_t[flag1 - 1];
			Amu2 =  Amu[flag1 - 1] + sigma_W_SST* Amu_t[flag1 - 1];
		}
		else {
			Amu1 = ( Amu[flag1] +  Amu[flag1 - 1] + sigma_K_SST*( Amu_t[flag1] +  Amu_t[flag1 - 1]))*0.5E0;
			Amu2 = ( Amu[flag1] +  Amu[flag1 - 1] + sigma_W_SST*( Amu_t[flag1] +  Amu_t[flag1 - 1]))*0.5E0;
		}

		//计算物理量（k, w）对坐标x, y的导数 （采用myjocabmyian变换）
		Dix = ( x1[flag2 - 1] +  x1[flag2] -  x1[flag3 - 1] -  x1[flag3])*0.25E0;
		Diy = ( y1[flag2 - 1] +  y1[flag2] -  y1[flag3 - 1] -  y1[flag3])*0.25E0;
		Djx =  x1[flag1] -  x1[flag1 - 1];
		Djy =  y1[flag1] -  y1[flag1 - 1];
		Ds = 1.E0 / (Dix*Djy - Djx*Diy);

		Dik = (Kt[flag2 - 1] + Kt[flag2] - Kt[flag3 - 1] - Kt[flag3])*0.25E0;
		Diw = (Wt[flag2 - 1] + Wt[flag2] - Wt[flag3 - 1] - Wt[flag3])*0.25E0;
		Djk = Kt[flag1] - Kt[flag1 - 1];
		Djw = Wt[flag1] - Wt[flag1 - 1];
		//
		kx = (Dik*Djy - Djk*Diy)*Ds;
		Wx = (Diw*Djy - Djw*Diy)*Ds;
		ky = (-Dik*Djx + Djk*Dix)*Ds;
		Wy = (-Diw*Djx + Djw*Dix)*Ds;

		Fluxk += Amu1*(kx*nj1 + ky*nj2)*sj / Re;
		Fluxw += Amu2*(Wx*nj1 + Wy*nj2)*sj / Re;
		__syncthreads();
	
		Res5[flag] -= Fluxk;
		Res6[flag] -= Fluxw;

		__syncthreads();
		flag = i*(ny + 1) + j - 1;
		Res5[flag] += Fluxk;
		Res6[flag] += Fluxw;
	}
}


//粘性系数虚网格上的值 （固壁边界采用反值，以保证固壁上的平均湍流粘性系数为0）
void Amut_boundary(int mBlock)
{
	int ib, ie, jb, je;
	Block_TYPE & B = Mesh[1].Block[mBlock];

	int nx = B.nx; int ny = B.ny;

	//Ghost Cell 点的 mut值为 内点mut值*（ - 1）(这样可以使壁面上mut = 0)

	for (int ksub = 1; ksub <= B.subface; ++ksub) {
		BC_MSG_TYPE & Bc = B.bc_msg[ksub];
		if (Bc.neighb == BC_Wall) {   //(粘性)壁面边界条件
			ib = Bc.ist; ie = Bc.iend;
			jb = Bc.jst; je = Bc.jend;

			if (Bc.face == 1) {   //i -
				for (int j = jb; j <= je - 1; ++j) {
					B.Amu_t[0 + LAP][j + LAP] = -B.Amu_t[1 + LAP][j + LAP];
				}
			}
			else if (Bc.face == 3) {   //i +
				for (int j = jb; j <= je - 1; ++j) {
					B.Amu_t[nx + LAP][j + LAP] = -B.Amu_t[nx - 1 + LAP][j + LAP];      //mut
				}
			}
			else if (Bc.face == 2) {   //j -
				for (int i = ib; i <= ie - 1; ++i) {
					B.Amu_t[i + LAP][0 + LAP] = -B.Amu_t[i + LAP][1 + LAP];       //mut
				}
			}
			else if (Bc.face == 4) {   //j +
				for (int i = ib; i <= ie - 1; ++i) {
					B.Amu_t[i + LAP][ny + LAP] = -B.Amu_t[i + LAP][ny - 1 + LAP];       //mut
				}
			}
		}
	}
}

