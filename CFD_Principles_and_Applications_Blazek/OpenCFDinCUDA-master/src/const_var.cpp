#include "const_var.h"

extern double PI = 3.1415926535897932;
extern double P_rato_limit = 2.0;
extern int Scheme_UD1 = 0, Scheme_NND2 = 1, Scheme_UD3 = 2, Scheme_WENO3 = 3, Scheme_MUSCL2 = 4, Scheme_MUSCL3 = 5, Scheme_OMUSCL2 = 6;
extern int Flux_Steger_Warming = 1, Flux_HLL = 2, Flux_HLLC = 3, Flux_Roe = 4, Flux_VanLeer = 5, Flux_Ausm = 6;
extern int Reconst_Original = 0, Reconst_Conservative = 1, Reconst_Characteristic = 2;
extern int BC_Wall = -10, BC_Farfield = -20, BC_Symmetry_or_slidewall = -30, BC_Inlet = -40, BC_Outlet = -50, BC_Move_Wall=-60;
extern int Time_Euler1 = 1, Time_RK3 = 3, Time_LU_SGS = 0, Time_Dual_LU_SGS = -1;
extern int Turbulence_NONE = 0, Turbulence_BL = 1, Turbulence_SA = 2, Turbulence_SST = 3;
extern int LAP = 2;		// 块和块之间的交叠区(overlap)宽度 （LAP=2最高支持4阶，LAP=3最高支持6阶，LAP=4最高支持8阶精度）
extern double PrT = 0.9;	//湍流Plandtl数
extern int Method_FVM = 0, Method_FDM = 1;   //有限体积法，差分法
extern int LFDM = 4;    //差分法块的边界网格（考虑到块 - 块连接处的网格的不光滑性，块边缘的4层网格不使用差分法）


