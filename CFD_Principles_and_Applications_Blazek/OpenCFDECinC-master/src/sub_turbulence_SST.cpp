#include "sub_turbulence_SST.h"
#include "common.h"

#include "Global_var.h"
#include <fstream>
#include<iomanip>


//计算各网格点到壁面的距离，SST模型需要使用该值
void comput_dw()
{
	Mesh_TYPE & MP = Mesh[1];	//最细的网格
	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {
		Block_TYPE & B = MP.Block[mBlock];
		int nx = B.nx;
		int ny = B.ny;
		allocMatrix(B.dw, nx, ny);
		for (int i = 0; i <= nx; ++i) {
			for (int j = 0; j <= ny; ++j) {
				B.dw[i][j] = 0;
			}
		}
	}

	//到壁面的距离，如数据文件存在，则读取
	std::ifstream fcin;
	fcin.open("dist_wall.dat");
	if (fcin.is_open()) {
		int NB;
		fcin >> NB;
		int nx1 = 1; int ny1 = 1;
		for (int i = 1; i <= NB; ++i) {
			fcin >> nx1 >> ny1;
		}
		for (int k = 1; k <= NB; ++k) {
			Block_TYPE & B = MP.Block[k];
			for (int j = 1; j <= B.ny - 1; ++j) {
				for (int i = 1; i <= B.nx - 1; ++i) {
					fcin >> B.dw[i][j];
				}
			}
		}
		fcin.close();
		return;
	}

	//如数据文件不存在，则创建
	//对于每个给定的点，搜索壁面各点，找出最小距离
	for (int mBlock = 1; mBlock <= MP.Num_Block; ++mBlock) {
		Block_TYPE & B = MP.Block[mBlock];
		int nx = B.nx;	int ny = B.ny;
		double **dw;
		allocMatrix(dw, nx, ny);
		for (int i = 1; i <= nx; ++i) {
			for (int j = 1; j <= ny; ++j) {
				dw[i][j] = 1.0E10;
				for (int m1 = 1; m1 <= MP.Num_Block; ++m1) {
					Block_TYPE & B1 = MP.Block[m1];
					for (int n1 = 1; n1 <= B1.subface; ++n1) {
						BC_MSG_TYPE & Bc1 = B1.bc_msg[n1];
						if (Bc1.neighb != BC_Wall) { continue; }
						for (int i1 = Bc1.ist; i1 <= Bc1.iend; ++i1) {
							for (int j1 = Bc1.jst; j1 <= Bc1.jend; ++j1) {
								double d1 = sqrt((B.x[i + LAP][j + LAP] - B1.x[i1 + LAP][j1 + LAP])*(B.x[i + LAP][j + LAP] - B1.x[i1 + LAP][j1 + LAP]) +
									(B.y[i + LAP][j + LAP] - B1.y[i1 + LAP][j1 + LAP])*(B.y[i + LAP][j + LAP] - B1.y[i1 + LAP][j1 + LAP]));
								if (d1 < dw[i][j]) { dw[i][j] = d1; }      //搜寻到壁面点最小的距离
							}
						}
					}
				}

			}
		}
		for (int i = 1; i <= nx - 1; ++i) {
			for (int j = 1; j <= ny - 1; ++j) {
				//中心点 （到壁面的距离） = 四周点的平均值
				B.dw[i][j] = (dw[i][j] + dw[i + 1][j] + dw[i][j + 1] + dw[i + 1][j + 1])*0.25e0;
			}
		}
		deleteMatrix(dw, nx);
	}

	//存储文件
	std::ofstream fcout;
	fcout << std::setprecision(12);
	fcout.open("dist_wall.dat");
	fcout << MP.Num_Block << std::endl;
	for (int k = 1; k <= MP.Num_Block; ++k) {
		fcout << MP.Block[k].nx << "  " << MP.Block[k].ny << "  ";
	}
	fcout << std::endl;
	for (int m = 1; m <= MP.Num_Block; ++m) {
		Block_TYPE &B = MP.Block[m];
		for (int j = 1; j <= B.ny - 1; ++j) {
			for (int i = 1; i <= B.nx - 1; ++i) {
				fcout << B.dw[i][j] << "  ";
			}
			fcout << std::endl;
		}
		fcout << std::endl;
	}
	fcout.close();
	//tecplot格式文件
	fcout.open("dw.plt");
	fcout << "variables=x,y,dw" << std::endl;
	for (int m = 1; m <= MP.Num_Block; ++m) {
		Block_TYPE & B = MP.Block[m];
		int nx = B.nx;	int ny = B.ny;
		fcout << "zone  i= " << nx - 1 << "  j=  " << ny - 1 << std::endl;
		for (int j = 1; j <= ny - 1; ++j) {
			for (int i = 1; i <= nx - 1; ++i) {
				fcout << B.x1[i + LAP][j + LAP] << "  " << B.y1[i + LAP][j + LAP] << "  " << B.dw[i][j] << std::endl;
			}
		}
	}
	fcout.close();
}



//SST k - w模型
//计算湍流粘性系数Amu_t;
//计算k方程及w方程的残差（右端项）;

void SST_kw(int nMesh, int mBlock, flow_var & fl)
{

	double si, sj, ni1, ni2, nj1, nj2, un, un1, un2, an1, an2, Amu1, Amu2;
	double Dix, Diy, Djx, Djy, Ds, Dik, Diw, Djk, Djw, Diu, Div, Dju, Djv, kx, ky, Wx, Wy, ux, vx, uy, vy;
	double  omega, Kws, CD_kw, arg1, arg2, arg3, f2, t11, t22, t12, Pk, Pk0;

	double ** Kt; double **  Wt; double ** f1;
	double ** Qk; double ** Qw;	double ** Fluxk;	double ** Fluxw;

	//模型系数(1为k - w模型的系数，在近壁区使用； 2为k - epsl模型的系数在远壁区使用）
	//SST是一个k - w与k - epsl模型的混合模型，通过开关函数f来切换
	const double sigma_k1_SST = 0.85E0, sigma_w1_SST = 0.5E0, beta1_SST = 0.075E0, Cw1_SST = 0.533E0;
	const double sigma_k2_SST = 1.E0, sigma_w2_SST = 0.856E0, beta2_SST = 0.0828E0, Cw2_SST = 0.440E0;
	const double a1_SST = 0.31E0, betas_SST = 0.09E0;
	double sigma_K_SST, sigma_W_SST, beta_SST, Cw_SST;

	Mesh_TYPE  & MP = Mesh[nMesh];
	Block_TYPE & B = MP.Block[mBlock];
	const int nx = B.nx; const int ny = B.ny;
	allocMatrix(f1, nx, ny);	allocMatrix(Qk, nx, ny); allocMatrix(Qw, nx, ny);
	allocMatrix(Fluxk, nx, ny);	allocMatrix(Fluxw, nx, ny);
	const int nx1 = nx + 2 * LAP - 1;	const int ny1 = ny + 2 * LAP - 1;
	//下面两个下标从 1-LAP 开始
	allocMatrix(Kt, nx1, ny1);	allocMatrix(Wt, nx1, ny1);

	//$OMP PARALLEL DEFAULT(PRIVATE) SHARED(nx, ny, B, Kt, Wt, f1, Qk, Qw, Fluxk, Fluxw, d, uu, v)

	//$OMP DO
	for (int i = 1; i <= nx1; ++i) {
		for (int j = 1; j <= ny1; ++j) {
			Kt[i][j] = B.U[i][j][5] / fl.d[i][j];
			Wt[i][j] = B.U[i][j][6] / fl.d[i][j];
		}
	}

	//$OMP END DO
	for (int i = 0; i <= nx; ++i) {
		for (int j = 0; j <= ny; ++j) {
			f1[i][j] = 0.0;
		}
	}

	//计算湍流粘性系数Amu_t;
	//计算函数f1 （区分近壁区与远壁区）
	//计算源项，导数采用2阶中心差分（利用Jocabian变换）
	//源项是k及w方程的主导项（比对流、扩散项更关键)
	//See Blazek's Book, section 7.2.3

	//$OMP DO
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			int myi = i + LAP;	int myj = j + LAP;
			//计算物理量对坐标x, y的导数，使用Jocabian变换
			Dix = (B.x1[myi + 1][myj] - B.x1[myi - 1][myj])*0.5E0;
			Diy = (B.y1[myi + 1][myj] - B.y1[myi - 1][myj])*0.5E0;
			Djx = (B.x1[myi][myj + 1] - B.x1[myi][myj - 1])*0.5E0;
			Djy = (B.y1[myi][myj + 1] - B.y1[myi][myj - 1])*0.5E0;
			Ds = 1.E0 / (Dix*Djy - Djx*Diy);

			Diu = (fl.uu[myi + 1][myj] - fl.uu[myi - 1][myj])*0.5E0;
			Div = (fl.v[myi + 1][myj] - fl.v[myi - 1][myj])*0.5E0;
			Dik = (Kt[myi + 1][myj] - Kt[myi - 1][myj])*0.5E0;
			Diw = (Wt[myi + 1][myj] - Wt[myi - 1][myj])*0.5E0;

			Dju = (fl.uu[myi][myj + 1] - fl.uu[myi][myj - 1])*0.5E0;
			Djv = (fl.v[myi][myj + 1] - fl.v[myi][myj - 1])*0.5E0;
			Djk = (Kt[myi][myj + 1] - Kt[myi][myj - 1])*0.5E0;
			Djw = (Wt[myi][myj + 1] - Wt[myi][myj - 1])*0.5E0;

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
			B.Amu[myi][myj] = B.Amu[myi][myj] * Re;
			omega = vx - uy;      //涡量
			arg2 = max(2.E0* sqrt(abs(Kt[myi][myj])) / (0.09*Wt[myi][myj] * B.dw[i][j] * Re), 500.E0*B.Amu[myi][myj] / (fl.d[myi][myj] * Wt[myi][myj] * B.dw[i][j] * B.dw[i][j] * Re*Re));

			f2 = tanh(arg2*arg2);

			//////Revised by Wang XiangYu
			B.Amu_t[myi][myj] = min(min(fl.d[myi][myj] * Kt[myi][myj] / Wt[myi][myj], a1_SST*fl.d[myi][myj] * Kt[myi][myj] * Re / (f2*abs(omega))), 100000.);
			//计算f1(识别是否为近壁区，近壁区趋近于1）
			Kws = 2.E0*(kx*Wx + ky*Wy)*fl.d[myi][myj] * sigma_w2_SST / (Wt[myi][myj] + 1.E-20);      //交叉输运项
			CD_kw = max(Kws, 1.E-20);
			arg3 = max(sqrt(abs(Kt[myi][myj])) / (0.09*Wt[myi][myj] * B.dw[i][j] * Re), 500.E0*B.Amu[myi][myj] / (fl.d[myi][myj] * Wt[myi][myj] * B.dw[i][j] * B.dw[i][j] * Re*Re));

			arg1 = min(arg3, 4.E0*fl.d[myi][myj] * sigma_w2_SST*Kt[myi][myj] / (CD_kw*B.dw[i][j] * B.dw[i][j]));
			f1[i][j] = tanh(arg1*arg1*arg1*arg1);             //开关函数，近壁区趋近于1，远壁区趋近于0  （用来切换k - w及k - epsl方程)


															  //湍应力 （使用了涡粘模型）
			t11 = ((4.E0 / 3.E0)*ux - (2.E0 / 3.E0)*vy)*B.Amu_t[myi][myj] - (2.E0 / 3.E0)*fl.d[myi][myj] * Kt[myi][myj] * Re;   //Blazek's Book, Eq. (7.25)
			t22 = ((4.E0 / 3.E0)*vy - (2.E0 / 3.E0)*ux)*B.Amu_t[myi][myj] - (2.E0 / 3.E0)*fl.d[myi][myj] * Kt[myi][myj] * Re;
			t12 = (uy + vx)*B.Amu_t[myi][myj];

			//湍能方程的源项（生成 - 耗散)
			//Pk = t11*ux + t22*vy + t12*(uy + vx)  //湍能生成项 （湍应力乘以应变率）
			Pk = B.Amu_t[myi][myj] * omega*omega;        //简化

			Pk0 = min(Pk, 20.E0*betas_SST*Kt[myi][myj] * Wt[myi][myj] * Re*Re);    //对湍能生成项进行限制，防止湍能过大

			Qk[i][j] = Pk0 / Re - betas_SST*fl.d[myi][myj] * Wt[myi][myj] * Kt[myi][myj] * Re;    //k方程的源项  （生成项 - 耗散项）

			Cw_SST = f1[i][j] * Cw1_SST + (1.E0 - f1[i][j])*Cw2_SST;    //模型系数，利用f1函数进行切换
			beta_SST = f1[i][j] * beta1_SST + (1.E0 - f1[i][j])*beta2_SST;    //模型系数，利用f1函数进行切换
			 //Qw[i][j] = Cw_SST*d[i][j]*Pk / (B.Amu_t[i][j] + 1.d - 20) / Re - beta_SST*d[i][j]*Wt[i][j]**2 * Re + (1.E0 - f1[i][j])*Kws / Re     //W方程的源项
			Qw[i][j] = Cw_SST*fl.d[myi][myj] * omega*omega / Re - beta_SST*fl.d[myi][myj] * Wt[myi][myj] * Wt[myi][myj] * Re + (1.E0 - f1[i][j])*Kws / Re;     //W方程的源项
		}
	}
	//$OMP END DO

	//对流项和扩散项
	//---- - i - direction----------------------------------------------------------------------------------
	//$OMP  DO
	for (int i = 1; i <= nx; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			int myi = i + LAP;	int myj = j + LAP;
			si = B.si[i][j];   //控制体边界长度
			ni1 = B.ni1[i][j]; ni2 = B.ni2[i][j];   //法方向

													//对流项，采用1阶迎风格式
			un1 = fl.uu[myi - 1][myj] * ni1 + fl.v[myi - 1][myj] * ni2;
			un2 = fl.uu[myi][myj] * ni1 + fl.v[myi][myj] * ni2;

			//1阶L - F 格式
			Fluxk[i][j] = -0.5E0*((un1 + abs(un1))*Kt[myi - 1][myj] + (un2 - abs(un2))*Kt[myi][myj])*si;
			Fluxw[i][j] = -0.5E0*((un1 + abs(un1))*Wt[myi - 1][myj] + (un2 - abs(un2))*Wt[myi][myj])*si;

			//粘性项（扩散项），采用2阶中心格式

			//格式系数，k - w与k - epsl格式系数之间选择，(f1作为切换开关函数)
			sigma_K_SST = f1[i][j] * sigma_k1_SST + (1.E0 - f1[i][j])*sigma_k2_SST;
			sigma_W_SST = f1[i][j] * sigma_w1_SST + (1.E0 - f1[i][j])*sigma_w2_SST;

			//界面上的值 = 两侧值的平均, 边界上的扩散系数 = 内侧的值
			if (i == 1) {
				Amu1 = B.Amu[myi][myj] + sigma_K_SST*B.Amu_t[myi][myj];         //扩散系数(k方程)
				Amu2 = B.Amu[myi][myj] + sigma_W_SST*B.Amu_t[myi][myj];         //扩散系数(w方程)
			}
			else if (i == nx) {
				Amu1 = B.Amu[myi - 1][myj] + sigma_K_SST*B.Amu_t[myi - 1][myj];        //扩散系数(k方程)
				Amu2 = B.Amu[myi - 1][myj] + sigma_W_SST*B.Amu_t[myi - 1][myj];        //扩散系数(w方程)
			}
			else {
				Amu1 = (B.Amu[myi - 1][myj] + B.Amu[myi][myj] + sigma_K_SST*(B.Amu_t[myi - 1][myj] + B.Amu_t[myi][myj]))*0.5E0;        //扩散系数(k方程), 界面上的值 = 两侧值的平均
				Amu2 = (B.Amu[myi - 1][myj] + B.Amu[myi][myj] + sigma_W_SST*(B.Amu_t[myi - 1][myj] + B.Amu_t[myi][myj]))*0.5E0;        //扩散系数(w方程)
			}

			//计算物理量（k, w）对坐标x, y的导数 （采用Jocabian变换）
			//----Jocabian系数 （物理坐标对计算坐标的导数, 用于计算物理量的导数）
			Dix = B.x1[myi][myj] - B.x1[myi - 1][myj];
			Diy = B.y1[myi][myj] - B.y1[myi - 1][myj];
			Djx = (B.x1[myi - 1][myj + 1] + B.x1[myi][myj + 1] - B.x1[myi - 1][myj - 1] - B.x1[myi][myj - 1])*0.25E0;
			Djy = (B.y1[myi - 1][myj + 1] + B.y1[myi][myj + 1] - B.y1[myi - 1][myj - 1] - B.y1[myi][myj - 1])*0.25E0;
			Ds = 1.E0 / (Dix*Djy - Djx*Diy);
			//物理量对计算坐标的导数
			Dik = Kt[myi][myj] - Kt[myi - 1][myj];
			Diw = Wt[myi][myj] - Wt[myi - 1][myj];
			Djk = (Kt[myi - 1][myj + 1] + Kt[myi][myj + 1] - Kt[myi - 1][myj - 1] - Kt[myi][myj - 1])*0.25E0;
			Djw = (Wt[myi - 1][myj + 1] + Wt[myi][myj + 1] - Wt[myi - 1][myj - 1] - Wt[myi][myj - 1])*0.25E0;
			//物理量对x, y坐标的导数
			kx = (Dik*Djy - Djk*Diy)*Ds;
			Wx = (Diw*Djy - Djw*Diy)*Ds;
			ky = (-Dik*Djx + Djk*Dix)*Ds;
			Wy = (-Diw*Djx + Djw*Dix)*Ds;
			//粘性应力及能量通量
			Fluxk[i][j] = Fluxk[i][j] + Amu1*(kx*ni1 + ky*ni2)*si / Re;
			Fluxw[i][j] = Fluxw[i][j] + Amu2*(Wx*ni1 + Wy*ni2)*si / Re;
		}
	}
	//$OMP END DO

	//$OMP  DO
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			B.Res[i][j][5] = Fluxk[i + 1][j] - Fluxk[i][j];
			B.Res[i][j][6] = Fluxw[i + 1][j] - Fluxw[i][j];
		}
	}
	//$OMP END DO


	//---- - j - direction----------------------------------------------------------------------------------
	
	//$OMP  DO
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny; ++j) {
			int myi = i + LAP; int myj = j + LAP;
			//边长，法方向
			sj = B.sj[i][j];
			nj1 = B.nj1[i][j]; nj2 = B.nj2[i][j];

			//对流项，采用1阶迎风格式 （L - F分裂）

			un1 = fl.uu[myi][myj - 1] * nj1 + fl.v[myi][myj - 1] * nj2;
			un2 = fl.uu[myi][myj] * nj1 + fl.v[myi][myj] * nj2;
			Fluxk[i][j] = -0.5E0*((un1 + abs(un1))*Kt[myi][myj - 1] + (un2 - abs(un2))*Kt[myi][myj])*sj;
			Fluxw[i][j] = -0.5E0*((un1 + abs(un1))*Wt[myi][myj - 1] + (un2 - abs(un2))*Wt[myi][myj])*sj;

			//粘性项
			//-------- - Vmyiscous term---------------------------------------------------------------------------- -
			sigma_K_SST = f1[i][j] * sigma_k1_SST + (1.E0 - f1[i][j])*sigma_k2_SST;
			sigma_W_SST = f1[i][j] * sigma_w1_SST + (1.E0 - f1[i][j])*sigma_w2_SST;

			if (j == 1) {
				Amu1 = B.Amu[myi][myj] + sigma_K_SST*B.Amu_t[myi][myj];
				Amu2 = B.Amu[myi][myj] + sigma_W_SST*B.Amu_t[myi][myj];
			}
			else if (j == ny) {
				Amu1 = B.Amu[myi][myj - 1] + sigma_K_SST*B.Amu_t[myi][myj - 1];
				Amu2 = B.Amu[myi][myj - 1] + sigma_W_SST*B.Amu_t[myi][myj - 1];
			}
			else {
				Amu1 = (B.Amu[myi][myj] + B.Amu[myi][myj - 1] + sigma_K_SST*(B.Amu_t[myi][myj] + B.Amu_t[myi][myj - 1]))*0.5E0;
				Amu2 = (B.Amu[myi][myj] + B.Amu[myi][myj - 1] + sigma_W_SST*(B.Amu_t[myi][myj] + B.Amu_t[myi][myj - 1]))*0.5E0;
			}

			//计算物理量（k, w）对坐标x, y的导数 （采用myjocabmyian变换）
			Dix = (B.x1[myi + 1][myj - 1] + B.x1[myi + 1][myj] - B.x1[myi - 1][myj - 1] - B.x1[myi - 1][myj])*0.25E0;
			Diy = (B.y1[myi + 1][myj - 1] + B.y1[myi + 1][myj] - B.y1[myi - 1][myj - 1] - B.y1[myi - 1][myj])*0.25E0;
			Djx = B.x1[myi][myj] - B.x1[myi][myj - 1];
			Djy = B.y1[myi][myj] - B.y1[myi][myj - 1];
			Ds = 1.E0 / (Dix*Djy - Djx*Diy);

			Dik = (Kt[myi + 1][myj - 1] + Kt[myi + 1][myj] - Kt[myi - 1][myj - 1] - Kt[myi - 1][myj])*0.25E0;
			Diw = (Wt[myi + 1][myj - 1] + Wt[myi + 1][myj] - Wt[myi - 1][myj - 1] - Wt[myi - 1][myj])*0.25E0;
			Djk = Kt[myi][myj] - Kt[myi][myj - 1];
			Djw = Wt[myi][myj] - Wt[myi][myj - 1];
			//
			kx = (Dik*Djy - Djk*Diy)*Ds;
			Wx = (Diw*Djy - Djw*Diy)*Ds;
			ky = (-Dik*Djx + Djk*Dix)*Ds;
			Wy = (-Diw*Djx + Djw*Dix)*Ds;

			Fluxk[i][j] += Amu1*(kx*nj1 + ky*nj2)*sj / Re;
			Fluxw[i][j] += Amu2*(Wx*nj1 + Wy*nj2)*sj / Re;
		}
	}												//$OMP END DO

													//残差 = 对流项 + 粘性项 + 源项

													//$OMP  DO
	for (int i = 1; i <= nx - 1; ++i) {
		for (int j = 1; j <= ny - 1; ++j) {
			int myi = i + LAP;	int myj = j + LAP;

			B.Res[i][j][5] += Fluxk[i][j + 1] - Fluxk[i][j] + Qk[i][j] * B.vol[i][j];
			B.Res[i][j][6] += Fluxw[i][j + 1] - Fluxw[i][j] + Qw[i][j] * B.vol[i][j];

			B.Amu[myi][myj] = B.Amu[myi][myj] / Re;
			B.Amu_t[myi][myj] = B.Amu_t[myi][myj] / Re;

		/*	printf("%d, %d\n", i, j);
			printf("%12.10f, %12.10f, %12.10f, %12.10f\n", B.Res[i][j][5], B.Res[i][j][6], B.Amu[myi][myj], B.Amu_t[myi][myj]);
			PAUSE;*/
		}
	}														//$OMP END DO
															//$OMP END PARALLEL

	deleteMatrix(f1, nx);	deleteMatrix(Qk, nx); deleteMatrix(Qw, nx);
	deleteMatrix(Fluxk, nx);	deleteMatrix(Fluxw, nx);

	deleteMatrix(Kt, nx1);	deleteMatrix(Wt, nx1);

	//设定湍流粘性系数虚网格的值
	Amut_boundary(nMesh, mBlock);

}


//粘性系数虚网格上的值 （固壁边界采用反值，以保证固壁上的平均湍流粘性系数为0）
void Amut_boundary(int nMesh, int mBlock)
{
	int ib, ie, jb, je;
	Block_TYPE & B = Mesh[nMesh].Block[mBlock];

	int nx = B.nx; int ny = B.ny;

	//Ghost Cell 点的 mut值为 内点mut值*（ - 1）(这样可以使壁面上mut = 0)

	for (int ksub = 1; ksub <= B.subface; ++ksub) {
		BC_MSG_TYPE & Bc = B.bc_msg[ksub];
		if (Bc.neighb == BC_Wall) {   //(粘性)壁面边界条件
			ib = Bc.ist; ie = Bc.iend;
			jb = Bc.jst; je = Bc.jend;

			if (Bc.face == 1) {   //i -
				for (int j = jb; j <= je - 1; ++j) {
					B.Amu_t[0+LAP][j+LAP] = -B.Amu_t[1+LAP][j+LAP];
				}
			}
			else if (Bc.face == 3) {   //i +
				for (int j = jb; j <= je - 1; ++j) {
					B.Amu_t[nx+LAP][j+LAP] = -B.Amu_t[nx - 1+LAP][j+LAP];      //mut
				}
			}
			else if (Bc.face == 2) {   //j -
				for (int i = ib; i <= ie - 1; ++i) {
					B.Amu_t[i+LAP][0+LAP] = -B.Amu_t[i+LAP][1+LAP];       //mut
				}
			}
			else if (Bc.face== 4) {   //j +
				for (int i = ib; i <= ie - 1; ++i) {
					B.Amu_t[i+LAP][ny+LAP] = -B.Amu_t[i+LAP][ny - 1+LAP];       //mut
				}
			}
		}
	}
}