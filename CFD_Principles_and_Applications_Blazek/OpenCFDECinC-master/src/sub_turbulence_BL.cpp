#include "sub_turbulence_BL.h"
#include "Global_var.h"
#include "common.h"
#include <cmath>

void BL_model_1d(int ny, double * yy, double *Amu, double *Amu_t, double * d, double * u, double *v, double *omiga);

void turbulence_model_BL(int nMesh, int mBlock, flow_var & fl)
{
	double ui, vi, uj, vj, ux, vx, uy, vy;
	double xi, yi, xj, yj, Ds, Jac, ix, iy, jx, jy, x0, y0;
	Block_TYPE & B = Mesh[nMesh].Block[mBlock];
	int nx1 = B.nx; int  ny1 = B.ny;

	//test if the block contains wall
	int kflag = 0;
	for (int ksub = 1; ksub <= B.subface; ++ksub) {
		if (B.bc_msg[ksub].neighb == BC_Wall)
			kflag = 1;
	}
	if (kflag == 0) return;    //No wall in this block

							   //This Block Contains Wall
	double ** omiga;	int ** flag1;
	allocMatrix(omiga, nx1, ny1);	allocMatrix(flag1, nx1, ny1);
	for (int i = 0; i <= nx1; ++i) {
		for (int j = 0; j <= ny1; ++j) {
			omiga[i][j] = 0.E0;
			flag1[i][j] = 0;
		}
	}
	int mm1 = nx1 + 2 * LAP - 1;	int nn1 = ny1 + 2 * LAP - 1;
	for (int i = 0; i <= mm1; ++i) {
		for (int j = 0; j <= nn1; ++j) {
			//double temp = B.Amu[i][j];
			B.Amu_t[i][j] = 0.E0;
		}
	}
	//---- - get Omiga(vorticity)  omiga = vx - uy at the cell's center ------
	//$OMP PARALLEL for( int DEFAULT(PRIVATE) SHARED(nx1, ny1, B, uu, v, omiga)
	for (int i = 1 + LAP; i <= nx1 - 1 + LAP; ++i) {
		for (int j = 1 + LAP; j <= ny1 - 1 + LAP; ++j) {
			xi = B.x1[i + 1][j] - B.x1[i - 1][j];
			yi = B.y1[i + 1][j] - B.y1[i - 1][j];
			xj = B.x1[i][j + 1] - B.x1[i][j - 1];
			yj = B.y1[i][j + 1] - B.y1[i][j - 1];
			Jac = 1.E0 / (xi*yj - xj*yi);
			ix = Jac*yj; iy = -Jac*xj; jx = -Jac*yi; jy = Jac*xi;   //Jocabian:  ksix, ksiy, itax, itay

			ui = fl.uu[i + 1][j] - fl.uu[i - 1][j];
			vi = fl.v[i + 1][j] - fl.v[i - 1][j];
			uj = fl.uu[i][j + 1] - fl.uu[i][j - 1];
			vj = fl.v[i][j + 1] - fl.v[i][j - 1];

			vx = vi*ix + vj*jx;
			uy = ui*iy + uj*jy;
			omiga[i - LAP][j - LAP] = abs(vx - uy);
		}
	}

	//$OMP end PARALLEL do int
	//----------------------------------------------------------------------
	for (int ksub = 1; ksub <= B.subface; ++ksub) {
		BC_MSG_TYPE & Bc = B.bc_msg[ksub];
		if (Bc.neighb == BC_Wall) {   //Wall boundary

			if (Bc.face == 1) {   //face of i +
								  //使用临时变量矩阵代替各个临时变量数组
								  //allocate( yy(nx1), Amu1d(nx1), Amut1d(nx1), d1d(nx1),   u1d(nx1),   v1d(nx1),  omiga1d(nx1))
								  //		tempVar[1],	tempVar[2], tempVar[3],   tempVar[4],tempVar[5], tempVar[6], tempVar[7],		
				double ** tempVar;	int tempVarNum = 8;
				allocMatrix(tempVar, tempVarNum, nx1);
				for (int inum = 0; inum <= tempVarNum; ++inum) {
					for (int jnum = 0; jnum <= nx1; ++jnum) {
						tempVar[inum][jnum] = 0;
					}
				}

				for (int j = Bc.jst; j <= Bc.jend - 1; ++j) {
					int myj = j + LAP;
					x0 = (B.x1[1+LAP][myj] + B.x1[0 + LAP][myj])*0.5E0;
					y0 = (B.y1[1 + LAP][myj] + B.y1[0 + LAP][myj])*0.5E0;
					for (int i = 1 + LAP; i <= nx1 - 1 + LAP; ++i) {
						int myi = i + LAP;
						tempVar[1][i] = sqrt((B.x1[myi][myj] - x0)*(B.x1[myi][myj] - x0) + (B.y1[myi][myj] - y0)*(B.y1[myi][myj] - y0));
						tempVar[5][i] = fl.uu[myi][myj];    tempVar[6][i] = fl.v[myi][myj];   tempVar[4][i] = fl.d[myi][myj];   tempVar[7][i] = omiga[i][j];
						tempVar[2][i] = B.Amu[myi][myj];
					}

					BL_model_1d(nx1, tempVar[1], tempVar[2], tempVar[3], tempVar[4], tempVar[5], tempVar[6], tempVar[7]);

					for (int i = 1; i <= nx1 - 1; ++i) {
						if (flag1[i][j] == 0) {
							flag1[i][j] = 1;
							B.Amu_t[i+LAP][j + LAP] = tempVar[3][i];
						}
						else {
							B.Amu_t[i + LAP][j + LAP] = B.Amu_t[i + LAP][j + LAP]<tempVar[3][i] ? B.Amu_t[i + LAP][j + LAP] : tempVar[3][i];
						}
					}
				}

				for (int i = 0; i <= mm1; ++i) {
					B.Amu_t[i][Bc.jst - 1 + LAP] = B.Amu_t[i][Bc.jst + LAP];
					B.Amu_t[i][Bc.jend + LAP] = B.Amu_t[i][Bc.jend - 1 + LAP];
				}
				for (int j = Bc.jst + LAP; j <= Bc.jend + LAP; ++j) {
					B.Amu_t[nx1 + LAP][j] = B.Amu_t[nx1 - 1 + LAP][j];
					B.Amu_t[LAP][j] = -B.Amu_t[1 + LAP][j];		//To set Amu_t = 0 in the wall
				}

				deleteMatrix(tempVar, tempVarNum);
			}
			else if (Bc.face == 3) {  //face i -
									  //使用临时变量矩阵代替各个临时变量数组
									  //allocate( yy(nx1), Amu1d(nx1), Amut1d(nx1), d1d(nx1),   u1d(nx1),   v1d(nx1),  omiga1d(nx1))
									  //		tempVar[1],	tempVar[2], tempVar[3],   tempVar[4],tempVar[5], tempVar[6], tempVar[7],		
				double ** tempVar;	int tempVarNum = 8;
				allocMatrix(tempVar, tempVarNum, nx1);
				for (int inum = 0; inum <= tempVarNum; ++inum) {
					for (int jnum = 0; jnum <= nx1; ++jnum) {
						tempVar[inum][jnum] = 0;
					}
				}

				for (int j = Bc.jst; j <= Bc.jend - 1; ++j) {
					int myj = j + LAP;
					x0 = (B.x1[nx1 - 1 + LAP][myj] + B.x1[nx1 + LAP][myj])*0.5E0;
					y0 = (B.y1[nx1 - 1 + LAP][myj] + B.y1[nx1 + LAP][myj])*0.5E0;

					for (int i = nx1 - 1; i >= 1; --i) {
						int i1 = nx1 - i;
						int myi = i + LAP;
						tempVar[1][i1] = sqrt((B.x1[myi][myj] - x0)*(B.x1[myi][myj] - x0) + (B.y1[myi][myj] - y0)*(B.y1[myi][myj] - y0));
						tempVar[5][i1] = fl.uu[myi][myj];    tempVar[6][i1] = fl.v[myi][myj];
						tempVar[4][i1] = fl.d[myi][myj];   tempVar[7][i1] = omiga[i][j];
						tempVar[2][i1] = B.Amu[myi][myj];
					}
					BL_model_1d(nx1, tempVar[1], tempVar[2], tempVar[3], tempVar[4], tempVar[5], tempVar[6], tempVar[7]);

					for (int i = nx1 - 1; i >= 1; --i) {
						int i1 = nx1 - i;
						int myi = i + LAP;
						if (flag1[i][j] == 0) {
							flag1[i][j] = 1;
							B.Amu_t[myi][myj] = tempVar[3][i1];
						}
						else {
							B.Amu_t[myi][myj] = B.Amu_t[myi][myj] < tempVar[3][i1] ? B.Amu_t[myi][myj] : tempVar[3][i1];
						}
					}
				}

				for (int i = LAP; i <= mm1 + LAP; ++i) {
					B.Amu_t[i][Bc.jst - 1 + LAP] = B.Amu_t[i][Bc.jst + LAP];
					B.Amu_t[i][Bc.jend + LAP] = B.Amu_t[i][Bc.jend - 1 + LAP];
				}
				for (int j = Bc.jst + LAP; j <= Bc.jend + LAP; ++j) {
					B.Amu_t[nx1 + LAP][j] = -B.Amu_t[nx1 - 1 + LAP][j];	//To set Amu_t = 0 in the wall
					B.Amu_t[LAP][j] = B.Amu_t[1 + LAP][j];
				}

				deleteMatrix(tempVar, tempVarNum);
			}
			else if (Bc.face == 2) {   //face of j -
									   //使用临时变量矩阵代替各个临时变量数组
									   //allocate( yy(nx1), Amu1d(nx1), Amut1d(nx1), d1d(nx1),   u1d(nx1),   v1d(nx1),  omiga1d(nx1))
									   //		tempVar[1],	tempVar[2], tempVar[3],   tempVar[4],tempVar[5], tempVar[6], tempVar[7],		
				double ** tempVar;	int tempVarNum = 8;
				allocMatrix(tempVar, tempVarNum, ny1);
				for (int inum = 0; inum <= tempVarNum; ++inum) {
					for (int jnum = 0; jnum <= ny1; ++jnum) {
						tempVar[inum][jnum] = 0;
					}
				}

				for (int i = Bc.ist; i <= Bc.iend - 1; ++i) {
					int myi = i + LAP;
					x0 = (B.x1[myi][1+LAP] + B.x1[myi][0+LAP])*0.5E0; y0 = (B.y1[myi][1+LAP] + B.y1[myi][0+LAP])*0.5E0;
					for (int j = 1; j <= ny1 - 1; ++j) {
						int myj = j + LAP;
						tempVar[1][j] = sqrt((B.x1[myi][myj] - x0)*(B.x1[myi][myj] - x0) + (B.y1[myi][myj] - y0)*(B.y1[myi][myj] - y0));
						tempVar[5][j] = fl.uu[myi][myj];    tempVar[6][j] = fl.v[myi][myj];
						tempVar[4][j] = fl.d[myi][myj];   tempVar[7][j] = omiga[i][j];
						tempVar[2][j] = B.Amu[myi][myj];
					}

					BL_model_1d(ny1, tempVar[1], tempVar[2], tempVar[3], tempVar[4], tempVar[5], tempVar[6], tempVar[7]);

					for (int j = 1; j <= ny1 - 1; ++j) {
						int myj = j + LAP;
						if (flag1[i][j] == 0) {
							flag1[i][j] = 1;
							B.Amu_t[myi][myj] = tempVar[3][j];
						}
						else {
							B.Amu_t[myi][myj] = B.Amu_t[myi][myj] < tempVar[3][j] ? B.Amu_t[myi][myj] : tempVar[3][j];
						}
					}
				}

				for (int i = Bc.ist + LAP; i <= Bc.iend + LAP; ++i) {
					B.Amu_t[i][LAP] = -B.Amu_t[i][1 + LAP];		//To set B.Amu_t = 0 in the wall
					B.Amu_t[i][ny1 + LAP] = B.Amu_t[i][ny1 - 1 + LAP];
				}
				for (int j = 1 + LAP; j <= nn1 + LAP; ++j) {
					B.Amu_t[Bc.ist - 1 + LAP][j] = B.Amu_t[Bc.ist + LAP][j];	//To set Amu_t = 0 in the wall
					B.Amu_t[Bc.iend + LAP][j] = B.Amu_t[Bc.iend - 1 + LAP][j];
				}

				deleteMatrix(tempVar, tempVarNum);

			}
			else {  //face j -
					//使用临时变量矩阵代替各个临时变量数组
					//allocate( yy(nx1), Amu1d(nx1), Amut1d(nx1), d1d(nx1),   u1d(nx1),   v1d(nx1),  omiga1d(nx1))
					//		tempVar[1],	tempVar[2], tempVar[3],   tempVar[4],tempVar[5], tempVar[6], tempVar[7],		
				double ** tempVar;	int tempVarNum = 8;
				allocMatrix(tempVar, tempVarNum, ny1);
				for (int inum = 0; inum <= tempVarNum; ++inum) {
					for (int jnum = 0; jnum <= ny1; ++jnum) {
						tempVar[inum][jnum] = 0;
					}
				}

				for (int i = Bc.ist; i <= Bc.iend - 1; ++i) {
					int myi = i + LAP;
					double x0 = (B.x1[myi][ny1 - 1+LAP] + B.x1[myi][ny1+LAP])*0.5E0;
					double y0 = (B.y1[myi][ny1 - 1+LAP] + B.y1[myi][ny1+LAP])*0.5E0;
					for (int j = ny1 - 1; j >= 1; --j) {
						int j1 = ny1 - j;
						int myj = j + LAP;
						tempVar[1][j1] = sqrt((B.x1[myi][myj] - x0)*(B.x1[myi][myj] - x0) + (B.y1[myi][myj] - y0)*(B.y1[myi][myj] - y0));
						tempVar[5][j1] = fl.uu[myi][myj];    tempVar[6][j1] = fl.v[myi][myj];
						tempVar[4][j1] = fl.d[myi][myj];   tempVar[7][j1] = omiga[i][j];
						tempVar[2][j1] = B.Amu[myi][myj];
					}

					BL_model_1d(ny1, tempVar[1], tempVar[2], tempVar[3], tempVar[4], tempVar[5], tempVar[6], tempVar[7]);

					for (int j = ny1 - 1; j >= 1; --j) {
						int j1 = ny1 - j;
						int myj = j + LAP;
						if (flag1[i][j] == 0) {
							flag1[i][j] = 1;
							B.Amu_t[myi][myj] = tempVar[3][j1];
						}
						else {
							B.Amu_t[myi][myj] = B.Amu_t[myi][myj] < tempVar[3][j] ? B.Amu_t[myi][myj] : tempVar[3][j];
						}
					}
				}
				for (int i = Bc.ist + LAP; i <= Bc.iend + LAP; ++i) {
					B.Amu_t[i][LAP] = B.Amu_t[i][1 + LAP];
					B.Amu_t[i][ny1 + LAP] = -B.Amu_t[i][ny1 - 1 + LAP];	//To set B.Amu_t = 0 in the wall
				}
				for (int j = 1 + LAP; j <= nn1 + LAP; ++j) {
					B.Amu_t[Bc.ist - 1 + LAP][j] = B.Amu_t[Bc.ist + LAP][j];
					B.Amu_t[Bc.iend + LAP][j] = B.Amu_t[Bc.iend - 1 + LAP][j];
				}

				deleteMatrix(tempVar, tempVarNum);
			}
		}
	}

	//取消限制条件(2010 - 10 - 12).很多工况出现粘性不足，产生虚假分离，无法收敛现象，需要加大粘性。
	//for( int j = 0, ny1
	//for( int i = 0, nx1
	//限制条件，避免计算出的湍流粘性系数过大
	//if (Amu_t[i,j].gt. 1.E0 / sqrt(Re)) Amu_t[i,j] = 1.E0 / sqrt(Re)
	//}
	//}


	//write(99, *) "zone i=", nx1 + 1, " j= ", ny1 + 1
	//for( int j = 0, ny1
	//for( int i = 0, nx1
	//write(99, "(4f20.10)") B.x1[i,j], B.y1[i,j], omiga[i,j], Amu_t[i,j]*Re
	//}
	//}												

		deleteMatrix(omiga, nx1);
		deleteMatrix(flag1, nx1);
}


//c------------------------------------------------------------------------
//B - L model of turbulence
//Ref:  Wilox DC.Turbulence Modeling for CFD(2nd Edition), p77
void BL_model_1d(int ny, double * yy, double *Amu, double *Amu_t, double * d, double * u, double *v, double *omiga)
{
	const double AP = 26., Ccp = 1.6, Ckleb = 0.3, Cwk = 0.25E0, AKT = 0.4, AK = 0.0168;  //Cwk = 1.E0
	double Tw, Ret, Fmax, etamax, Udif, etap, FF, Fwak, bl, Fkleb, Visti, Visto, uu;

	Tw = abs(Amu[1] * omiga[1]);
	for (int j = 1; j <= ny - 1; ++j) {
		if (abs(Amu[j] * omiga[j]) > Tw)  Tw = abs(Amu[j] * omiga[j]);
	}
	Ret = sqrt(d[1] * Tw) / Amu[1];

	Fmax = 0.E0;  etamax = 0.E0;  Udif = 0.E0;

	//Ny_boundary = Ny
	int Ny_boundary = ny - 1;   //////修改 2011 - 10 - 18

								//临时修改，防止搜索范围过大，造成ymax, Fmax 出现过大值
								//if (Ny.gt. 10) {
								//Ny_boundary = Ny - 5
								//}else
								//Ny_boundary = Ny
								//}
								//----------------------------------
	for (int j = 1; j <= Ny_boundary; ++j) {
		double uu = sqrt(u[j] * u[j] + v[j] * v[j]);
		if (uu > Udif) Udif = uu;
		etap = yy[j] * Ret;
		FF = yy[j] * abs(omiga[j])*(1.E0 - exp(-etap / AP));

		if (FF>Fmax) {
			Fmax = FF;
			etamax = yy[j];    //某些位置算出的值偏大
		}

		//if (FF.gt.Fmax) {
		//Fmax = FF
		//etamax = yy(j)
		//}else
		//goto 100    //Find the first peak of F(y)   //重要的修改 //////
		//}
	}
	///*100*/      continue;
	int IFlag = 0;
	Fwak = min(etamax*Fmax, Cwk*etamax*Udif*Udif / Fmax);
	for (int j = 1; j <= ny - 1; ++j) {
		etap = Ret*yy[j];
		bl = AKT*yy[j] * (1.E0 - exp(-etap / AP));
		Visti = d[j] * bl*bl*abs(omiga[j]);
		Fkleb = 1.E0 / (1.E0 + 5.5E0* pow((Ckleb*yy[j] / etamax), 6));
		Visto = AK*Ccp*d[j] * Fwak*Fkleb;
		if (abs(Visto)<abs(Visti)) IFlag = 1;
		if (IFlag == 0) {
			Amu_t[j] = Visti;
		}
		else {
			Amu_t[j] = Visto;
		}
	}
}


