#include"Global_var.h"

extern Mesh_TYPE * Mesh=nullptr;       //主数据 “网格”
extern int Num_Mesh = 1;                            //网格的套数
extern int Nvar=4;
extern int Kstep_save=0, If_viscous = 0, Iflag_turbulence_model = 0, Iflag_init = 0;
extern int Iflag_Scheme = 0, Iflag_Flux = 0, Iflag_local_dt = 0, IFlag_Reconstruction = 0, Time_Method = 0, Kstep_show = 0, Num_Threads = 0;
extern double Ma = 0, Re = 0, gamma = 0, Pr = 0, AoA = 0, Cp = 0, Cv = 0, t_end = 0, p_outlet = 0, T_inf = 0, Twall = 0, vt_inf = 0, Kt_inf = 0, Wt_inf = 0;
extern double dt_global = 0, CFL = 0, dtmax = 0, dtmin = 0;                                        //与时间步长有关的量
extern double Res_Inner_Limit = 0;     //内迭代残差下限
extern double Pre_Step_Mesh[4] = {0};
extern double Nstep_Inner_Limit = 0;

extern int Iflag1=0;
