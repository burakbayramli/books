#pragma once

//This file define the global parameter include 
//physical parameter and geometry parameters


#ifndef _GLOBAL_VAR_H_
#define _GLOBAL_VAR_H_

//#include<stdio.h>
//#include<stdlib.h>

#include "const_var.h"

//边界链接信息
struct  BC_MSG_TYPE
{
	int f_no, face, ist, iend, jst, jend, neighb, subface, orient;
};

//------------------------------------网格块--------------------------------------
//结构体：存放网格块信息，包含几何变量及物理变量的信息。
struct Block_TYPE
{
	int Block_no, nx, ny, subface;           //块号；网格数nx, ny；子面数
	int FVM_FDM;		//有限体积法或者是有限差分法
	double ** x, **y;	//(x,y) : coordinate of vortex; 
	double **x1, **y1;	//(x1, y1) : coordinate of cell center
	double *** U, ***Un, ***Un1;     //守恒变量(本时间步及前1, 2个时间步的值), conversation variables
	double *** Res;		//残差
	double ** dt;	//（局部）时间步长
	double ***	QF;		//强迫函数（多重网格法中粗网格使用）
	double ***	deltU;   //守恒变量的差值, dU = U(n + 1) - U(n)  多重网格使用
	double *** dU;       //守恒变量的插值dU = U(n + 1) - U(n), LU - SGS方法中使用
	double **Amu, **Amu_t;	//层流粘性系数、湍流粘性系数
//几何量: vol控制体面积; si, sj i, j - 方向控制体边界长度; (ni1, ni2) i - 方向控制体边界法方向; (nj1, nj2) j - 方向控制体边界法方向;
	double ** vol, **si, ** sj, **ni1, ** ni2, ** nj1, ** nj2;
	double ** Lci, ** Lcj, ** Lvi, ** Lvj;       //谱半径(Lci, Lcj i - , j - 方向无粘项谱半径; Lvi, Lvj i - , j - 方向粘性项谱半径)
	double ** dw;                //到壁面的距离 （k - w SST模型中使用）
	BC_MSG_TYPE * bc_msg;    //边界链接信息                                        
};


//-------------------------- - 网格--------------------------------------------------------
//(如单重网格，只有1套；如多重网格，可以有多套)
struct Mesh_TYPE
{
	//数据结构“网格”； 包含几何变量及物理变量信息
	int Mesh_no, Num_Block, Num_Cell, Kstep;        //网格编号(1号为最细网格，2号为粗网格， 3号为更粗网格...)，网格块数，网格数目, 时间步
	int Nvar;	//变量（方程）数目，如使用k - w模型，则有6个变量 （稀网格不使用湍模型，因而变量仍是4个）
	double Res_max[7], Res_rms[7], tt;                 //最大残差，均方根残差, 推进的时间
	Block_TYPE * Block;     //“网格块”  （从属于“网格”）
	
	//控制参数，用于控制数值方法、通量技术、湍流模型等
	//这些控制参数从属于“网格”，不同“网格”可以采用不同的计算方法、湍流模型等。	 （例如，粗网格用低精度方法，粗网格不使用湍流模型, ...）
	int Iflag_turbulence_model, Iflag_Scheme, IFlag_flux, IFlag_Reconstruction;
};              
//-------------------------------------------------------------------------------------------- -

//global variables                                       各子程序均可见的全局变量
//----------------------------------------------------------------------------
extern Mesh_TYPE * Mesh;       //主数据 “网格”
extern int Num_Mesh;                            //网格的套数
extern int Nvar;
extern int Kstep_save, If_viscous, Iflag_turbulence_model, Iflag_init;
extern int Iflag_Scheme, Iflag_Flux, Iflag_local_dt, IFlag_Reconstruction, Time_Method, Kstep_show, Num_Threads;
extern double Ma, Re, gamma, Pr, AoA, Cp, Cv, t_end, p_outlet, T_inf, Twall, vt_inf, Kt_inf, Wt_inf;
extern double dt_global, CFL, dtmax, dtmin;                                        //与时间步长有关的量
extern double Res_Inner_Limit;     //内迭代残差下限
extern double Pre_Step_Mesh[4];
extern double Nstep_Inner_Limit;                 //构建初值时，粗网格预迭代步数； 内迭代步数限制

extern int Iflag1;
//---------------------------------------------- -
//全局控制参数，控制数值方法、通量技术及湍流模型等 （有些只对最细网格有效）
//Nvar方程（变量）的数目， 如使用BL模型Nvar = 4;  使用SA模型 Nvar = 5, 如使用K - W SST模型 Nvar = 6 (4个基本方程 + k方程 + w方程）
	//控制变量  If_viscous = 0 Euler方程，1 N_S方程；
	//Iflag_turbulence_model 湍流模型（BL, SA, SST）;
//Iflag_Scheme 数值格式；
//Iflag_flux 通量技术；
//Iflag_local_dt 是否采用局部时间步长;
//Num_Threads OpenMP采用的线程数
//global parameter(for all Meshes)                     流动参数, 对全体“网格”都适用
//Ma: Mach数;  Re: Reynolds数; Pr: Prandtl数; Cp, Cv: 定压、定容比热;
//t_end: End time 计算结束的时间; P_outlet: 出口压力（亚声速内流计算必须给定, 无量纲量）;  Twall: 壁温（有量纲量，单位K）
//T_inf : 来流温度(有量纲值，单位K), 在surthland公式中使用;
//Kt_inf, Wt_inf: 来流湍动能、湍能比耗散率(Amut_inf = Kt_inf / Wt_inf), SST模型的初值及入口边界条件使用
//vt_inf : 来流的湍流粘性系数（与层流粘性系数之比）， SA模型使用



#define debug  true

void outputDebug();

extern bool USEGPU;
extern int * transferInt_dev;
extern double * transferDouble_dev;
extern double * x1_dev;
extern double * y1_dev;
extern double * x_dev;
extern double * y_dev;


#endif // _GLOBAL_VAR_H_

