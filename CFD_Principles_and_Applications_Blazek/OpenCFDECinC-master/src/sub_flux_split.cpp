#include "sub_flux_split.h"
#include "Global_Var.h"
#include "common.h"
#include <cmath>


//---------------------------------------------------------- -
//Code by Cofludy according to Leng Yan`s code 
void  Flux_Van_Leer_1Da(double *QL, double * QR, double *Flux, const double gamma)
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


