//---- - The core subroutines : Comput Residual : includes the inviscous and viscous flux-------- -
//Copyright by Li Xinliang, LHD, Institute of Mechanics, CAS.lixl@imech.ac.cn
//code by cofludy  (USTC; e-mail : cofludy@gmail.com) 
//Ver 1.1

#include "Global_var.h"
#include "sub_Residual.h"
#include "sub_flux_split.h"
#include "sub_NS_singlegid.h"
#include "common.h"
#include "sub_turbulence_BL.h"
#include "sub_turbulence_SST.h"
//#include "Flow_Var.h"

#include <cmath>

#include"kernelResidual.h"


void comput_duvtpckw(int nMesh, int mBlock, flow_var & fl);
void Residual_FVM(int nMesh, int mBlock, flow_var & fl);
void  Residual(int nMesh, int mBlock, flow_var & fl);

//void comput_Lvc(int nMesh, int mBlock);
void comput_dt(int nMesh, int mBlock);

void Reconstuction_Characteristic(double U0[5][5], double *UL, double * UR, double gamma, int Iflag_Scheme);
void Reconstuction_conservative(double  U0[5][5], double * UL, double * UR, double gamma, int  Iflag_Scheme);
void Reconstuction_original(double U0[5][5], double * UL, double * UR, double gamma, int Iflag_Scheme);


void scheme_fP(double & uL, double u1, double u2, double u3, double u4, int Iflag_Scheme);
void scheme_fm(double & UR, double u1, double u2, double u3, double u4, int Iflag_Scheme);




template<typename T>
T minmod(T a, T b) {
	T minmod;
	if (a*b < 0) {
		minmod = 0;
	}
	else {
		minmod = abs(a) <= abs(b) ? a : b;
	}
	return minmod;
}

//------------------------------------------------------------------ -
//利用原始变量进行平均 （用于计算角点的值）
void U_average(double * U1, double * U2, double * U0, const double gamma)
{
	double d1, uu1, v1, p1, d2, uu2, v2, p2, d0, uu0, v0, p0;
	d1 = U1[1]; uu1 = U1[2] / d1; v1 = U1[3] / d1;
	p1 = (U1[4] - (uu1*U1[2] + v1*U1[3])*0.5E0)*(gamma - 1.E0); //density, velocity, pressure
	
	d2 = U2[1]; uu2 = U2[2] / d2; v2 = U2[3] / d2; 
	p2 = (U2[4] - (uu2*U2[2] + v2*U2[3])*0.5E0)*(gamma - 1.E0);  //density, velocity, pressure
	
	d0 = (d1 + d2)*0.5E0; uu0 = (uu1 + uu2)*0.5E0; v0 = (v1 + v2)*0.5E0; p0 = (p1 + p2)*0.5E0;

	U0[1] = d0; U0[2] = d0*uu0; U0[3] = d0*v0; 
	U0[4] = p0 / (gamma - 1.E0) + (d0*uu0*uu0 + d0*v0*v0)*0.5E0;
}

// 计算残差（网格的全部块)
void Comput_Residual_one_mesh(int nMesh)
{
	Mesh_TYPE & MP = Mesh[nMesh];            //本层网格
	int NV = MP.Nvar;
	for (int i = 1; i <= NV; ++i){
		MP.Res_max[i]=0;
		MP.Res_rms[i]=0;
	}

	//------ - Dual Time LU - SGS----------------------
	//仅第1步Sfac = 1 / 3 (因假设U(n - 1) = U(n)), 其余均为1 / 2
	double Sfac = 0;
	if (Iflag1 == 0) {
		Iflag1 = 1;
		Sfac = 1.e0 / (3.E0*dt_global);
	}
	else {
		Sfac = 1.e0 / (2.E0*dt_global);
	}

	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {
		Block_TYPE & B = MP.Block[mBlock];                 //第nMesh 重网格的第mBlock块
		int nx = B.nx; int  ny = B.ny;

		//分配临时变量
		//allocate(d(0:nx, 0 : ny), uu(0:nx, 0 : ny), v(0:nx, 0 : ny), T(0:nx, 0 : ny), cc(0:nx, 0 : ny), p(0:nx, 0 : ny))   //Bug found, 2012 - 5 - 1
		flow_var  fl;
		int nx1 = nx + 2 * LAP - 1;	int ny1 = ny + 2 * LAP - 1;	//下标从1-LAP开始
		allocMatrix(fl.d, nx1, ny1);	allocMatrix(fl.uu, nx1, ny1);
		allocMatrix(fl.v, nx1, ny1);	allocMatrix(fl.T, nx1, ny1);
		allocMatrix(fl.p, nx1, ny1);	

		allocMatrix(fl.cc, nx - 1, ny - 1);

		allocMatrix(fl.Fluxi, nx, ny, 4);	allocMatrix(fl.Fluxj, nx, ny, 4);

		//--------------------------------------------------------------------------------------
		comput_duvtpckw(nMesh, mBlock, fl);    //计算基本量 d, u, v, T, p, cc, Kt, Wt
		//--------------------------------------------------------------------------------------
		//求解N - S方程的最核心模块
		//计算一个网格块的残差（右端项）; 第nMesh 重网格的第mBlock块  （湍流模型也在该块中计算）

		//Residual(nMesh, mBlock, fl);
		beforeKernelResidual(mBlock, fl); 

		//双时间步 LU - SGS方法，添加项
		if (Time_Method == Time_Dual_LU_SGS) {
			for (int i = 1; i <= nx - 1; ++i) {
				for (int j = 1; j <= ny - 1; ++j) {
					for (int m = 1; m <= Nvar; ++m) {
						B.Res[i][j][m] = B.Res[i][j][m] - (3.e0*B.U[i + LAP][j + LAP][m] - 4.e0*B.Un[i][j][m] + B.Un1[i][j][m])*B.vol[i][j] * Sfac;
					}
				}
			}
		}

		//--------------------------------------------------------------------------------------
		//统计最大残差和均方根残差(OpenMP中使用了规约变量)
		double Res_max[7] = { 0 };	double Res_rms[7] = { 0 };

		//$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(MP, B, NV) REDUCTION(MAX: Res_max) REDUCTION(+: Res_rms)
		for (int i = 1; i <= nx - 1; ++i) {
			for (int j = 1; j <= ny - 1; ++j) {
				//------------------------------------------------------------------------------------------ -
				//时间推进
				for (int m = 1; m <= NV; ++m) {
					double 	Res = B.Res[i][j][m];
					//--------------------------------------------------------------------------------------------------
					if (abs(Res) > Res_max[m])  Res_max[m] = abs(Res);          //最大残差
					Res_rms[m] = Res_rms[m] + Res*Res;                              //均方根残差
				}
			}
		}
		//$OMP END PARALLEL DO
		for (int m = 1; m <= NV; ++m) {
			MP.Res_max[m] = max(MP.Res_max[m], Res_max[m]);          //最大残差
			MP.Res_rms[m] = MP.Res_rms[m] + Res_rms[m];                //全部网格点的总均方根残差
		}

		//----------------------------------------------------------------------------------------
		comput_Lvc(nMesh, mBlock, fl);  //计算无粘性及粘性项的谱半径
		comput_dt(nMesh, mBlock);        //计算(当地) 时间步长
		
		//删除掉临时变量
		deleteMatrix(fl.d, nx1);  deleteMatrix(fl.uu, nx1);	deleteMatrix(fl.v, nx1);
		deleteMatrix(fl.T, nx1);	deleteMatrix(fl.p, nx1);
		deleteMatrix(fl.cc, nx-1);
		deleteMatrix(fl.Fluxi, nx, ny);	deleteMatrix(fl.Fluxj, nx, ny);
	}

	for (int m = 1; m <= NV; ++m) {
		MP.Res_rms[m] = sqrt(MP.Res_rms[m] / (1.e0*MP.Num_Cell));   //全部网格点的总均方根残差
	}
}

	
//----------------------------------------------------------
//利用守恒变量，计算基本量(d, u, v, T, p, c)
//----------------------------------------------------------
void comput_duvtpckw(int nMesh, int mBlock, flow_var & fl)
{
	Mesh_TYPE & MP = Mesh[nMesh];
	Block_TYPE & B = MP.Block[mBlock];        //第nMesh 重网格的第mBlock块
	double 	p00 = 1.e0 / (gamma*Ma*Ma);
	int nx = B.nx; int ny = B.ny;

	//$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, j)
	for (int i = 1; i <= nx + 2 * LAP-1; ++i) {
		for (int j = 1; j <= ny + 2 * LAP - 1; ++j) {
			fl.d[i][j] = B.U[i][j][1];
			fl.uu[i][j] = B.U[i][j][2] / fl.d[i][j];
			fl.v[i][j] = B.U[i][j][3] / fl.d[i][j];
			fl.T[i][j] = (B.U[i][j][4] - 0.5e0*fl.d[i][j] * (fl.uu[i][j] * fl.uu[i][j] + fl.v[i][j] * fl.v[i][j])) / (Cv*fl.d[i][j]);
			fl.p[i][j] = p00*fl.d[i][j] * fl.T[i][j];

			if (fl.T[i][j]< 1.0e-5) {
				printf("\n T <1.d-5 at // i= %d, j=%d, mBlock= %d, T= %f \n", i, j, mBlock, fl.T[i][j]);
				PAUSE;
				exit(1);
			}
			if (B.U[i][j][1] != B.U[i][j][1] || B.U[i][j][2] != B.U[i][j][2] || B.U[i][j][3] != B.U[i][j][3] || B.U[i][j][4] != B.U[i][j][4]) {
				printf("ERROR! in comput_duvtpckw! mBlock= %d, i= %d, j=%d", mBlock, i, j);
				PAUSE;
			}
		}
	}

	//$OMP END PARALLEL DO
	//printf("%f   %f    %f ", fl.T[1][1], fl.d[1][1], fl.uu[1][1]);
	//PAUSE;
	//$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i, j)
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			if (fl.T[i+LAP][j+LAP]< 1.0e-5) {
				printf("T <1.d-5 at // i= %d, j=%d, mBlock= %d, T= %f \n", i, j, mBlock, fl.T[i+LAP][j+LAP]);
				PAUSE;
				exit(1);
			}
			fl.cc[i][j] = sqrt(fl.T[i+LAP][j+LAP]) / Ma;
		}
	}
	//$OMP END PARALLEL DO
}

//计算第nMesh套网格的第mBlock块的残差 （右端项）
void  Residual(int nMesh, int mBlock, flow_var & fl)
{
	Mesh_TYPE & MP = Mesh[nMesh];
	Block_TYPE & B = MP.Block[mBlock];        //第nMesh 重网格的第mBlock块
	int nx = B.nx;	 int ny = B.ny;

	//计算粘性系数(包括利用湍流模型计算湍流粘性系数)
	if (If_viscous == 1) {

		get_viscous(nMesh, mBlock,fl);         //计算层流粘性系数

		int mm1 = nx + 2 * LAP - 1;	int nn1 = ny + 2 * LAP - 1;
		for (int i = 1; i <= mm1; ++i) {
			for (int j = 1; j <= nn1; ++j) {
				//double temp = B.Amu[i][j];
				//printf("%f   \n", temp);
				B.Amu_t[i][j] = 0.E0;             //湍流粘性系数清零
			}
		}

		//湍流模型，计算湍流粘性系数Amu_t  （BL, SA, SST模型）
		if (MP.Iflag_turbulence_model == Turbulence_BL) {	
			turbulence_model_BL(nMesh, mBlock, fl);
			//PAUSE;
		}
		else if (MP.Iflag_turbulence_model == Turbulence_SA) {
			//turbulence_model_SA(nMesh, mBlock);
			PAUSE;
		}
		else if (MP.Iflag_turbulence_model == Turbulence_SST) {
			SST_kw(nMesh, mBlock,fl);
			//PAUSE;
		}
	}			
	//有限体积法计算残差(如该块使用差分法，则只计算临近块边界的4层网格）
	Residual_FVM(nMesh, mBlock, fl);

	//利用有限差分法计算残差(临近块边界的4层网格除外)
	if (B.FVM_FDM == Method_FDM) {
		//Residual_FDM(nMesh, mBlock);
		PAUSE;
	}

}


//------------------------------------------------------------------------------
//使用有限体积法计算残差(残差 = 右端项 = 净流量); 是求解N - S方程的核心模块;

//Flux(= inviscous flux + viscous flux)
void Residual_FVM(int nMesh, int mBlock, flow_var & fl)
{
	Mesh_TYPE & MP = Mesh[nMesh];
	Block_TYPE & B = MP.Block[mBlock];        //第nMesh 重网格的第mBlock块
	int nx = B.nx;	 int ny = B.ny;

	const int Scheme = MP.Iflag_Scheme;         //数值格式 （不同网格上采用不同格式）
	const int IFlux = MP.IFlag_flux;            //通量技术
	const int Reconstruction = MP.IFlag_Reconstruction;  //重构方式

	//$OMP PARALLEL DEFAULT(PRIVATE) SHARED(nx, ny, MP, B, d, uu, v, p, T, Fluxi, Fluxj, Scheme, IFlux, Reconstruction, If_viscous, gamma, Cp, Pr)

	//$OMP DO
	for (int i = 1; i <= nx; ++i) {
		for (int j = 1; j <= ny; ++j) {
			for (int m = 1; m <= 4; ++m) {
				fl.Fluxi[i][j][m] = 0.E0;  //清零
				fl.Fluxj[i][j][m] = 0.E0;
			}
		}
	}

	double UL[5], UR[5], UL2[5], UR2[5], QL[5], QR[5],Flux0[5];
	double U0[5][5];
	//$OMP ENDDO
	//---- - i - direction----------------------------------------------------------------------------------

	//$OMP for(
	for (int i = 1; i <= nx; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {

			//如采用差分法计算，则中心区不使用有限体积法计算
			if (B.FVM_FDM == Method_FDM && (i >= LFDM + 2 && i <= nx - LFDM - 1)
				&& (j >= LFDM + 2 && j <= ny - LFDM - 1)) {
				continue;		//跳过计算 （有限差分法的计算区域）
			}

			double si = B.si[i][j];   //控制体边界长度
			double ni1 = B.ni1[i][j]; double ni2 = B.ni2[i][j];   //法方向
																  //---- - 无粘项---------- - Inviscous term------------------------------------------------------------ -

																  //---- - 选择重构（插值）界面值的 方式  （使用原始变量、守恒变量或特征变量重构）
			if (Reconstruction == Reconst_Original) {
				for (int m = 1; m <= 4; ++m) {
					U0[1][m] = fl.d[i - 3 + m + LAP][j + LAP];
					U0[2][m] = fl.uu[i - 3 + m + LAP][j + LAP];
					U0[3][m] = fl.v[i - 3 + m + LAP][j + LAP];
					U0[4][m] = fl.p[i - 3 + m + LAP][j + LAP];
				}
				Reconstuction_original(U0, UL, UR, gamma, Scheme);
			}
			else if (Reconstruction == Reconst_Conservative) {
				for (int k = 1; k <= 4; ++k) {
					for (int m = 1; m <= 4; ++m) {
						U0[k][m] = B.U[i - 3 + m + LAP][j + LAP][k];
					}
				}
				Reconstuction_conservative(U0, UL, UR, gamma, Scheme);
			}
			else {
				for (int k = 1; k <= 4; ++k) {
					for (int m = 1; m <= 4; ++m) {
						U0[k][m] = B.U[i - 3 + m + LAP][j + LAP][k];
					}
				}
				Reconstuction_Characteristic(U0, UL, UR, gamma, Scheme);
			}

			//------ - 坐标旋转到垂直于界面 （法向 - 切向 坐标系）
			QL[1] = UL[1]; QL[2] = UL[2] * ni1 + UL[3] * ni2; QL[3] = -UL[2] * ni2 + UL[3] * ni1; QL[4] = UL[4]; //密度、压力、法向速度、切向速度 （左值）
			QR[1] = UR[1]; QR[2] = UR[2] * ni1 + UR[3] * ni2; QR[3] = -UR[2] * ni2 + UR[3] * ni1; QR[4] = UR[4]; //密度、压力、法向速度、切向速度 （右值）

																												 //------ - 选择通量分裂方法----(Steger_Warming, Van Leer, Roe, AUSM, HLL, HLLC----
			if (IFlux == Flux_Steger_Warming) {
				//Flux_steger_warming_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_HLL) {
				//Flux_HLL_HLLC_1D(QL, QR, Flux0, gamma, 0);
			}
			else if (IFlux == Flux_HLLC) {
				//Flux_HLL_HLLC_1D(QL, QR, Flux0, gamma, 1);
			}
			else if (IFlux == Flux_VanLeer) {
				Flux_Van_Leer_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_Ausm) {
				//Flux_Ausm_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_Roe) {
				//Flux_Roe_1D(QL, QR, Flux0, gamma);
			}

			//------------------------------------------------
			//算出通量 （变换回x - y坐标系）
			fl.Fluxi[i][j][1] = -Flux0[1] * si;                            //质量通量
			fl.Fluxi[i][j][2] = -(Flux0[2] * ni1 - Flux0[3] * ni2)*si;         //x - 方向动量通量
			fl.Fluxi[i][j][3] = -(Flux0[2] * ni2 + Flux0[3] * ni1)*si;         //y - 方向动量通量
			fl.Fluxi[i][j][4] = -Flux0[4] * si;                            //能量通量

			if (If_viscous == 0) { continue; }   //如求解Euler方程（无粘），则无需粘性项计算

												 //--i - 方向无粘通量计算结束，计算i - 方向粘性通量
												 //-------------------------------------------------------------------------------------------------------- -
												 //---------- - Viscous term-------------------------------------------------------------------------------- -
												 //扩散系数（粘性系数、热传导系数） = 界面两侧值的平均 （边界处采用单侧值)
			double Amu1 = 0; double Amk1 = 0;
			if (i == 1) {
				Amu1 = B.Amu[i + LAP][j + LAP] + B.Amu_t[i + LAP][j + LAP];                  //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*(B.Amu[i + LAP][j + LAP] / Pr + B.Amu_t[i + LAP][j + LAP] / PrT);     //热传导系数
			}
			else if (i == nx) {
				Amu1 = B.Amu[i - 1 + LAP][j + LAP] + B.Amu_t[i - 1 + LAP][j + LAP];                  //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*(B.Amu[i - 1 + LAP][j + LAP] / Pr + B.Amu_t[i - 1 + LAP][j + LAP] / PrT);     //热传导系数
			}
			else {
				Amu1 = (B.Amu[i - 1 + LAP][j + LAP] + B.Amu[i + LAP][j + LAP] + B.Amu_t[i - 1 + LAP][j + LAP] + B.Amu_t[i + LAP][j + LAP])*0.5E0;                 //粘性系数(层流 + 湍流), 界面上的值 = 两侧值的平均
				Amk1 = Cp*((B.Amu[i - 1 + LAP][j + LAP] + B.Amu[i + LAP][j + LAP]) / Pr + (B.Amu_t[i - 1 + LAP][j + LAP] + B.Amu_t[i + LAP][j + LAP]) / PrT)*0.5e0;   //热传导系数
			}

			//----Jocabian系数 （物理坐标对计算坐标的导数, 用于计算物理量的导数）
			int myi = i + LAP;	int myj = j + LAP;
			double Dix = B.x1[myi][myj] - B.x1[myi - 1][myj];
			double Diy = B.y1[myi][myj] - B.y1[myi - 1][myj];
			double Djx = (B.x1[myi - 1][myj + 1] + B.x1[myi][myj + 1] - B.x1[myi - 1][myj - 1] - B.x1[myi - 1][myj - 1])*0.25E0;
			double Djy = (B.y1[myi - 1][myj + 1] + B.y1[myi][myj + 1] - B.y1[myi - 1][myj - 1] - B.y1[myi - 1][myj - 1])*0.25E0;
			double Ds = 1.E0 / (Dix*Djy - Djx*Diy);
			//物理量对计算坐标的导数
			double Diu = fl.uu[myi][myj] - fl.uu[myi - 1][myj];
			double Div = fl.v[myi][myj] - fl.v[myi - 1][myj];
			double DiT = fl.T[myi][myj] - fl.T[myi - 1][myj];
			double Dju = (fl.uu[myi - 1][myj + 1] + fl.uu[myi][myj + 1] - fl.uu[myi - 1][myj - 1] - fl.uu[myi - 1][myj - 1])*0.25E0;
			double Djv = (fl.v[myi - 1][myj + 1] + fl.v[myi][myj + 1] - fl.v[myi - 1][myj - 1] - fl.v[myi - 1][myj - 1])*0.25E0;
			double DjT = (fl.T[myi - 1][myj + 1] + fl.T[myi][myj + 1] - fl.T[myi - 1][myj - 1] - fl.T[myi - 1][myj - 1])*0.25E0;
			//物理量对x, y坐标的导数
			double ux = (Diu*Djy - Dju*Diy)*Ds;
			double vx = (Div*Djy - Djv*Diy)*Ds;
			double Tx = (DiT*Djy - DjT*Diy)*Ds;
			double uy = (-Diu*Djx + Dju*Dix)*Ds;
			double vy = (-Div*Djx + Djv*Dix)*Ds;
			double Ty = (-DiT*Djx + DjT*Dix)*Ds;
			//粘性应力及能量通量
			double u1 = (fl.uu[myi][myj] + fl.uu[myi - 1][myj])*0.5E0;
			double v1 = (fl.v[myi][myj] + fl.v[myi - 1][myj])*0.5E0;
			double t11 = ((4.E0 / 3.E0)*ux - (2.E0 / 3.E0)*vy)*Amu1;
			double t22 = ((4.E0 / 3.E0)*vy - (2.E0 / 3.E0)*ux)*Amu1;
			double t12 = (uy + vx)*Amu1;
			double E1 = u1*t11 + v1*t12 + Amk1*Tx;
			double E2 = u1*t12 + v1*t22 + Amk1*Ty;
			//添加粘性通量
			/*fl.Fluxi[i - 1][j - 1][2] = fl.Fluxi[i - 1][j - 1][2] + (t11*ni1 + t12*ni2)*si;
			fl.Fluxi[i - 1][j - 1][3] = fl.Fluxi[i - 1][j - 1][3] + (t12*ni1 + t22*ni2)*si;
			fl.Fluxi[i - 1][j - 1][4] = fl.Fluxi[i - 1][j - 1][4] + (E1*ni1 + E2*ni2)*si;*/
			fl.Fluxi[i][j][2] +=(t11*ni1 + t12*ni2)*si;
			fl.Fluxi[i][j][3] +=(t12*ni1 + t22*ni2)*si;
			fl.Fluxi[i][j][4] +=(E1*ni1 + E2*ni2)*si;
			//printf("%f, %f, \n%f, %f", fl.Fluxi[i][j][1], fl.Fluxi[i][j][2], fl.Fluxi[i][j][3], fl.Fluxi[i][j][4]);
		}
	}
	//$OMP !

	//= == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == =
	//j - 方向的无粘及粘性通量
	//---------------------------------------- - j - direction------------------------------------------------------------ -
	//$OMP DO
	for(int i=1; i<=nx-1; ++i){
		for(int j=1; j<=ny;++j){

			if (B.FVM_FDM == Method_FDM && (i>=LFDM + 2 && i<=nx - LFDM - 1) 
				&&  (j>=LFDM + 2 && j<=ny - LFDM - 1)) {
				continue;	//跳过计算 （有限差分法的计算区域）
			}    

					//边长，法方向
			double sj = B.sj[i][j];
			double nj1 = B.nj1[i][j]; double nj2 = B.nj2[i][j];

			//---- - Inviscous term--------------------------------------------
			if (Reconstruction == Reconst_Original) {
				for (int m = 1; m <= 4; ++m) {
					U0[1][m] = fl.d[i+ LAP][j - 3 + m + LAP];
					U0[2][m] = fl.uu[i+ LAP][j - 3 + m + LAP];
					U0[3][m] = fl.v[i  + LAP][j - 3 + m + LAP];
					U0[4][m] = fl.p[i + LAP][j - 3 + m + LAP];
				}
				Reconstuction_original(U0, UL, UR, gamma, Scheme);
			}
			else if (Reconstruction == Reconst_Conservative) {
				for (int k = 1; k <= 4; ++k) {
					for (int m = 1; m <= 4; ++m) {
						U0[k][m] = B.U[i  + LAP][j - 3 + m + LAP][k];
					}
				}
				Reconstuction_conservative(U0, UL, UR, gamma, Scheme);
			}
			else {
				for (int k = 1; k <= 4; ++k) {
					for (int m = 1; m <= 4; ++m) {
						U0[k][m] = B.U[i  + LAP][j - 3 + m + LAP][k];
					}
				}
				Reconstuction_Characteristic(U0, UL, UR, gamma, Scheme);
			}

			//------ - 坐标旋转到垂直于界面 （法向 - 切向 坐标系）
			QL[1] = UL[1]; QL[2] = UL[2] * nj1 + UL[3] * nj2; QL[3] = -UL[2] * nj2 + UL[3] * nj1; QL[4] = UL[4]; //密度、压力、法向速度、切向速度 （左值）
			QR[1] = UR[1]; QR[2] = UR[2] * nj1 + UR[3] * nj2; QR[3] = -UR[2] * nj2 + UR[3] * nj1; QR[4] = UR[4]; //密度、压力、法向速度、切向速度 （右值）

																												 //------ - 选择通量分裂方法----(Steger_Warming, Van Leer, Roe, AUSM, HLL, HLLC----
			if (IFlux == Flux_Steger_Warming) {
				//Flux_steger_warming_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_HLL) {
				//Flux_HLL_HLLC_1D(QL, QR, Flux0, gamma, 0);
			}
			else if (IFlux == Flux_HLLC) {
				//Flux_HLL_HLLC_1D(QL, QR, Flux0, gamma, 1);
			}
			else if (IFlux == Flux_VanLeer) {
				Flux_Van_Leer_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_Ausm) {
				//Flux_Ausm_1Da(QL, QR, Flux0, gamma);
			}
			else if (IFlux == Flux_Roe) {
				//Flux_Roe_1D(QL, QR, Flux0, gamma);
			}
			//------------------------------------------------
			//算出通量 （变换回x - y坐标系）
			fl.Fluxj[i][j][1] = -Flux0[1] * sj;                            //质量通量
			fl.Fluxj[i][j][2] = -(Flux0[2] * nj1 - Flux0[3] * nj2)*sj;         //x - 方向动量通量
			fl.Fluxj[i][j][3] = -(Flux0[2] * nj2 + Flux0[3] * nj1)*sj;         //y - 方向动量通量
			fl.Fluxj[i][j][4] = -Flux0[4] * sj;                            //能量通量


			if (If_viscous == 0) { continue; }  //如求解Euler方程（无粘），则无需粘性项计算
												//-------- - Viscous term---------------------------------------------------------------------------- -
												//扩散系数（粘性系数、热传导系数） = 界面两侧值的平均 （边界处采用单侧值)
			double Amu1 = 0;	double Amk1 = 0;
			if (j == 1) {
				Amu1 = B.Amu[i + LAP][j + LAP] + B.Amu_t[i + LAP][j + LAP];
				Amk1 = Cp*(B.Amu[i + LAP][j + LAP] / Pr + B.Amu_t[i + LAP][j + LAP] / PrT);   //热传导系数
			}
			else if (j == ny) {
				Amu1 = B.Amu[i + LAP][j - 1 + LAP] + B.Amu_t[i + LAP][j - 1 + LAP];
				Amk1 = Cp*(B.Amu[i + LAP][j - 1 + LAP] / Pr + B.Amu_t[i + LAP][j - 1 + LAP] / PrT);   //热传导系数
			}
			else {
				Amu1 = (B.Amu[i + LAP][j + LAP] + B.Amu[i + LAP][j - 1 + LAP] + B.Amu_t[i + LAP][j + LAP] + B.Amu_t[i + LAP][j - 1 + LAP])*0.5E0;
				Amk1 = Cp*((B.Amu[i + LAP][j - 1 + LAP] + B.Amu[i + LAP][j + LAP]) / Pr + (B.Amu_t[i + LAP][j - 1 + LAP] + B.Amu_t[i + LAP][j + LAP]) / PrT)*0.5E0;   //热传导系数
			}

			int myi = i + LAP;	int myj = j + LAP;

			double Dix = (B.x1[myi + 1][myj - 1] + B.x1[myi + 1][myj] - B.x1[myi - 1][myj - 1] - B.x1[myi - 1][myj])*0.25E0;
			double Diy = (B.y1[myi + 1][myj - 1] + B.y1[myi + 1][myj] - B.y1[myi - 1][myj - 1] - B.y1[myi - 1][myj])*0.25E0;
			double Djx = B.x1[myi][myj] - B.x1[myi][myj - 1];
			double Djy = B.y1[myi][myj] - B.y1[myi][myj - 1];

			double Ds = 1.E0 / (Dix*Djy - Djx*Diy);

			double Diu = (fl.uu[myi + 1][myj - 1] + fl.uu[myi + 1][myj] - fl.uu[myi - 1][myj - 1] - fl.uu[myi - 1][myj])*0.25E0;
			double Div = (fl.v[myi + 1][myj - 1] + fl.v[myi + 1][myj] - fl.v[myi - 1][myj - 1] - fl.v[myi - 1][myj])*0.25E0;
			double DiT = (fl.T[myi + 1][myj - 1] + fl.T[myi + 1][myj] - fl.T[myi - 1][myj - 1] - fl.T[myi - 1][myj])*0.25E0;
			double Dju = fl.uu[myi][myj] - fl.uu[myi][myj - 1];
			double Djv = fl.v[myi][myj] - fl.v[myi][myj - 1];
			double DjT = fl.T[myi][myj] - fl.T[myi][myj - 1];
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
			double	u1 = (fl.uu[myi][myj] + fl.uu[myi][myj - 1])*0.5E0;
			double v1 = (fl.v[myi][myj] + fl.v[myi][myj - 1])*0.5E0;

			double E1 = u1*t11 + v1*t12 + Amk1*Tx;
			double E2 = u1*t12 + v1*t22 + Amk1*Ty;

			fl.Fluxj[i][j][2] += (t11*nj1 + t12*nj2)*sj;
			fl.Fluxj[i][j][3] += (t12*nj1 + t22*nj2)*sj;
			fl.Fluxj[i][j][4] += (E1*nj1 + E2*nj2)*sj;
		}
	}
	//$OMP }
	//------ - 计算残差 （净流量）----------------------------------------------------
	//不含强迫函数 （多重网格时，需添加强迫函数）
	//$OMP DO
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			if (B.FVM_FDM == Method_FDM && (i >= LFDM + 1 && i <= nx - LFDM - 1)
				&& (j >= LFDM + 1 && j <= ny - LFDM - 1)) {
				continue;		//跳过计算 （有限差分法的计算区域）
			}
			for (int m = 1; m <= 4; ++m) {
				B.Res[i][j][m] = fl.Fluxi[i + 1][j][m] - fl.Fluxi[i][j][m] + fl.Fluxj[i][j + 1][m] - fl.Fluxj[i][j][m];
			}
		}
	}
	//$OMP ENDDO
	//$OMP END PARALLEL
}

//--------------------------------------------------------------------------
//利用原始变量重构
void Reconstuction_original(double U0[5][5], double * UL, double * UR, double gamma, int Iflag_Scheme)
{
	//U0(m, k) : k = 1, 4 for  i - 2, i - 1, i, i + 1;   m = 1, 4 for d, uu, v, p
	for (int k = 1; k <= 4; ++k) {
		scheme_fP(UL[k], U0[k][1], U0[k][2], U0[k][3], U0[k][4], Iflag_Scheme);
		scheme_fm(UR[k], U0[k][1], U0[k][2], U0[k][3], U0[k][4], Iflag_Scheme);
	}
}

//利用守恒变量重构
void Reconstuction_conservative(double  U0[5][5], double * UL, double * UR, double gamma, int  Iflag_Scheme)
{
	//real * 8::U0(4, 4), UL(4), UR(4), QL(4), QR(4), gamma
	//U0(m, k) : k = 1, 4 for  i - 2, i - 1, i, i + 1; m for the conservative variables U0(1, m) = d, U0(2, m) = d*u, ....
	double QL[5] = { 0 }; double QR[5] = { 0 };
	for (int k = 1; k <= 4; ++k) {
		scheme_fP(QL[k], U0[k][1], U0[k][2], U0[k][3], U0[k][4], Iflag_Scheme);
		scheme_fm(QR[k], U0[k][1], U0[k][2], U0[k][3], U0[k][4], Iflag_Scheme);
	}
	UL[1] = QL[1]; UL[2] = QL[2] / UL[1]; UL[3] = QL[3] / UL[1];
	UL[4] = (QL[4] - (UL[2] * QL[2] + UL[3] * QL[3])*0.5E0)*(gamma - 1.E0);  //density, velocity, pressure and sound speed
	UR[1] = QR[1]; UR[2] = QR[2] / UR[1]; UR[3] = QR[3] / UR[1];
	UR[4] = (QR[4] - (UR[2] * QR[2] + UR[3] * QR[3])*0.5E0)*(gamma - 1.E0);  //find a bug, removed

}

//利用特征变量重构
void Reconstuction_Characteristic(double U0[5][5], double *UL, double * UR, double gamma, int Iflag_Scheme)
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
		scheme_fP(VL[m], V0[1][m], V0[2][m], V0[3][m], V0[4][m], Iflag_Scheme);
		scheme_fm(VR[m], V0[1][m], V0[2][m], V0[3][m], V0[4][m], Iflag_Scheme);
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
void scheme_fP(double &UL, double u1, double u2, double u3, double u4, int Iflag_Scheme)
{
	const double k = 1.E0, b = 2.E0;
	const double k3 = 1.E0 / 3.E0, ep = 1.E-6;

	if (Iflag_Scheme == Scheme_UD1) {
		UL = u2;//1阶迎风格式
	}
	else if (Iflag_Scheme == Scheme_UD3) {
		UL = (-u1 + 5.E0*u2 + 2.E0*u3) / 6.E0;//UD 3nd order  3阶迎风格式
	}
	else if (Iflag_Scheme == Scheme_NND2) {
		UL = u2 + 0.5E0*minmod(u2 - u1, u3 - u2);//NND 2nd order  2阶NND
	}
	else if (Iflag_Scheme == Scheme_WENO3) {  //WENO3 scheme
		double IS1 = (u2 - u1)*(u2 - u1);    double IS2 = (u3 - u2)*(u3 - u2);
		double a1 = 1.E0 / (3.E0*(1.E-6 + IS1)*(1.E-6 + IS1));   
		double a2 = 2.E0 / (3.E0*(1.E-6 + IS2)*(1.E-6 + IS2));
		double w1 = a1 / (a1 + a2);   double w2 = a2 / (a1 + a2);
		UL = w1*(-u1 / 2.E0 + 3.E0*u2 / 2.E0) + w2*(u2 / 2.E0 + u3 / 2.E0);//3阶WENO
	}
	else if (Iflag_Scheme == Scheme_MUSCL2) {         //2阶MUSCL(Minmod限制器）
		UL = u2 + 0.25E0*((1.E0 - k)*minmod(u2 - u1, b*(u3 - u2)) + (1.E0 + k)*minmod(u3 - u2, b*(u2 - u1)));//MUSCL
	}
	else if (Iflag_Scheme == Scheme_MUSCL3) {         //3阶MUSCL(Van Albada限制器)
		double up = u3 - u2; double um = u2 - u1;//1阶 前差、后差
		double s = (2.E0*up*um + ep) / (up*up + um*um + ep);//Van Albada限制器 （光滑区，前差与后差接近，该值接近1）
		UL = u2 + 0.25E0*s*((1.E0 - k3*s)*um + (1.E0 + k3*s)*up);//3阶MUSCL(光滑区逼近3阶迎风)
	}
	else if (Iflag_Scheme == Scheme_OMUSCL2) {         //2阶优化的MUSCL 格式(Developed by Leng Yan)
		double r1 = (u2 - u1 + ep) / (u3 - u2 + ep); 
		double r2 = (u3 - u2 + ep) / (u4 - u3 + ep);
		double f1 = 0.8E0 - 0.175E0 / r2 + 0.375E0*r1;
		double f = max(0.E0, min(2.E0, f1, 2.E0*r1));
		UL = u2 + 0.5E0*f*(u3 - u2);//2阶优化的MUSCL: OMUSCL2
	}
}

//数值格式，构造UR = U(j + 1 / 2, R); u1 = u(j - 1), u2 = u(j), u3 = u(j + 1), u4 = u(j + 2)
void scheme_fm(double & UR, double u1, double u2, double u3, double u4, int Iflag_Scheme)
{
	const double k = 1.E0 / 3.0, b = 1.E0;
	const double k3 = 1.E0 / 3.E0, ep = 1.E-6;

	if (Iflag_Scheme == Scheme_UD1) {
		UR = u3;                              //1阶迎风
	}
	else if (Iflag_Scheme == Scheme_UD3) {
		UR = (2.E0*u2 + 5.E0*u3 - u4) / 6.E0;     //UD 3nd order
	}
	else if (Iflag_Scheme == Scheme_NND2) {
		UR = u3 - 0.5E0*minmod(u3 - u2, u4 - u3);    //NND 2nd order
	}
	else if (Iflag_Scheme == Scheme_WENO3) {   //WENO3
		double IS1 = (u3 - u4)*(u3 - u4);   double IS2 = (u2 - u3)*(u2 - u3);
		double a1 = 1.E0 / (3.E0*(1.E-6 + IS1)*(1.E-6 + IS1));
		double a2 = 2.E0 / (3.E0*(1.E-6 + IS2)*(1.E-6 + IS2));
		double w1 = a1 / (a1 + a2);     double w2 = a2 / (a1 + a2);
		UR = w1*(-u4 / 2.E0 + 3.E0*u3 / 2.E0) + w2*(u3 / 2.E0 + u2 / 2.E0);       //WENO 3nd order
	}
	else if (Iflag_Scheme == Scheme_MUSCL2) {
		UR = u3 - 0.25E0*((1.E0 - k)*minmod(u4 - u3, b*(u3 - u2)) + (1.E0 + k)*minmod(u3 - u2, b*(u4 - u3)));   //MUSCL
	}
	else if (Iflag_Scheme == Scheme_MUSCL3) {      //3阶MUSCL(Van Albada限制器)
		double up = u4 - u3; double um = u3 - u2;                             //前差、后差
		double s = (2.E0*up*um + ep) / (up*up + um*um + ep);
		UR = u3 - 0.25E0*s*((1.E0 - k3*s)*up + (1.E0 + k3*s)*um);
	}
	else if (Iflag_Scheme == Scheme_OMUSCL2) {         //2阶优化的MUSCL 格式(Developed by Leng Yan)
		double r1 = (u4 - u3 + ep) / (u3 - u2 + ep); double r2 = (u3 - u2 + ep) / (u2 - u1 + ep);
		double f1 = 0.8E0 - 0.175E0 / r2 + 0.375E0*r1;
		double f = max(0.E0, min(2.E0, f1, 2.E0*r1));
		UR = u3 - 0.5E0*f*(u3 - u2);                               //2阶优化的MUSCL: OMUSCL2
	}
}
//------------------------------------------------------------------------------

//----------------------------------------------------------------
//分子粘性系数的计算
void get_viscous(int nMesh, int mBlock, flow_var &fl)
{
	Block_TYPE & B = Mesh[nMesh].Block[mBlock];

	//Ref_Amu_T0 = 288.15d0
	//T_inf （来流）参考温度
	const double Tsb = 110.4E0 / T_inf;
	int	nx = B.nx + LAP; int ny = B.ny + LAP;
	for (int j = LAP; j <= ny; ++j) {
		for (int i = LAP; i <= nx; ++i) {
			B.Amu[i][j] = 1.E0 / Re*(1.E0 + Tsb)*sqrt(fl.T[i][j] * fl.T[i][j] * fl.T[i][j]) / (Tsb + fl.T[i][j]);   //sutherland equation
			//printf("AMU=== %15.10f \n", B.Amu[i][j]);
		}
	}
}