// Clase estática que implementa el algoritmo que calcula la posición de los
// satélites en un momento dado y a partir de los datos ofrecidos por los
// archivos .tle
package sgp;

class SGP
{

	static void SGP(Satelite sat)
	{
		if (sat.ideep == 0) // Selección del modelo "Deep-Space" adecuado
			SGP4(sat);
		else
			SDP4(sat);
	}

	static void SGP4(Satelite sat)
	{
		int i;
		double cosuk,sinuk,rfdotk,vx,vy,vz,ux,uy,uz,xmy,xmx,
		    cosnok,sinnok,cosik,sinik,rdotk,xinck,xnodek,uk,rk,
		    cos2u,sin2u,u,sinu,cosu,betal,rfdot,rdot,r,pl,elsq,
		    esine,ecose,epw,temp6,temp5,temp4,temp3,cosepw,sinepw,
		    capu,ayn,xlt,aynl,xll,axn,xn,beta,xl,e,a,tfour,temp2,
		    tcube,delm,delomg,templ,tempe,tempa,xnode,tsq,xmp,temp,
		    omega,xnoddf,omgadf,xmdf,x,y,z,xdot,ydot,zdot,temp1;

		double tsince = (sat.tiempoJuliano - sat.julian_epoch) * Cte.xmnpda;

		// Recover original mean motion (xnodp) and semimajor axis (aodp)
		// from input elements.
  		if (sat.iflag == 1)
		{
			// Initialization
			sat.a1 = Math.pow(Cte.xke/sat.xno,Cte.tothrd);
			sat.cosio = Math.cos(sat.xincl);
			sat.theta2 = sat.cosio*sat.cosio;
			sat.x3thm1 = (3.0*sat.theta2) - 1.0;
			sat.eosq = sat.eo*sat.eo;
			sat.betao2 = 1.0 - sat.eosq;
			sat.betao = Math.sqrt(sat.betao2);
			sat.del1 = 1.5*Cte.ck2*sat.x3thm1/(sat.a1*sat.a1*sat.betao*sat.betao2);
			sat.ao = sat.a1*(1.0 - sat.del1*(0.5*Cte.tothrd + sat.del1*(1.0 + 134.0/81.0*sat.del1)));
			sat.delo = 1.5*Cte.ck2*sat.x3thm1/(sat.ao*sat.ao*sat.betao*sat.betao2);
			sat.xnodp = sat.xno/(1.0 + sat.delo);
			sat.aodp = sat.ao/(1.0 - sat.delo);

			// For perigee less than 220 kilometers, the isimp flag is set and
			// the equations are truncated to linear variation in sqrt a and
			// quadratic variation in mean anomaly.  Also, the c3 term, the
			// delta omega term, and the delta m term are dropped.

			sat.isimp = 0;
			if ((sat.aodp*(1.0 - sat.eo)/Cte.ae) < (220.0/Cte.xkmper + Cte.ae))
				sat.isimp = 1;
				// For perigee below 156 km, the values of s and qoms2t are altered.

			sat.s4 = Cte.s;
			sat.qoms24 = Cte.qoms2t;
			sat.perige = (sat.aodp*(1.0 - sat.eo) - Cte.ae)*Cte.xkmper;
			if (!(sat.perige >= 156.0))
			{
				sat.s4 = sat.perige - 78.0;
				if (!(sat.perige > 98.0))
					sat.s4 = 20.0;
				sat.qoms24 = Math.pow((120.0 - sat.s4)*Cte.ae/Cte.xkmper,4.0);
				sat.s4 = sat.s4/Cte.xkmper + Cte.ae;
			}

			sat.pinvsq = 1.0/(sat.aodp*sat.aodp*sat.betao2*sat.betao2);
			sat.tsi = 1.0/(sat.aodp - sat.s4);
			sat.eta = sat.aodp*sat.eo*sat.tsi;
			sat.etasq = sat.eta*sat.eta;
			sat.eeta = sat.eo*sat.eta;
			sat.psisq = Math.abs(1.0 - sat.etasq);
			sat.coef = sat.qoms24*Math.pow(sat.tsi,4.0);
			sat.coef1 = sat.coef/Math.pow(sat.psisq,3.5);
			sat.c2 = sat.coef1*sat.xnodp*(sat.aodp*(1.0 + 1.5*sat.etasq + sat.eeta*(4.0 + sat.etasq))
				+ 0.75*Cte.ck2*sat.tsi/sat.psisq*sat.x3thm1*(8.0 + 3.0*sat.etasq*(8.0 + sat.etasq)));
			sat.c1 = sat.bstar*sat.c2;
			sat.sinio = Math.sin(sat.xincl);
			sat.a3ovk2 = (-Cte.xj3)/Cte.ck2*Math.pow(Cte.ae,3.0);
			sat.c3 = sat.coef*sat.tsi*sat.a3ovk2*sat.xnodp*Cte.ae*sat.sinio/sat.eo;
			sat.x1mth2 = 1.0 - sat.theta2;
			sat.c4 = 2.0*sat.xnodp*sat.coef1*sat.aodp*sat.betao2*(sat.eta*(2.0 + 0.5*sat.etasq)
				+ sat.eo*(0.5 + 2.0*sat.etasq) - 2.0*Cte.ck2*sat.tsi/(sat.aodp*sat.psisq)
				*(-3.0*sat.x3thm1*(1.0 - 2.0*sat.eeta + sat.etasq*(1.5 - 0.5*sat.eeta))
				+ 0.75*sat.x1mth2*(2.0*sat.etasq - sat.eeta*(1.0 + sat.etasq))*Math.cos(2.0*sat.omegao)));
			sat.c5 = 2.0*sat.coef1*sat.aodp*sat.betao2*(1.0 + 2.75*(sat.etasq + sat.eeta) + sat.eeta*sat.etasq);
			sat.theta4 = sat.theta2*sat.theta2;
			temp1 = 3.0*Cte.ck2*sat.pinvsq*sat.xnodp;
			temp2 = temp1*Cte.ck2*sat.pinvsq;
			temp3 = 1.25*Cte.ck4*sat.pinvsq*sat.pinvsq*sat.xnodp;
			sat.xmdot = sat.xnodp + 0.5*temp1*sat.betao*sat.x3thm1
				+ 0.0625*temp2*sat.betao*(13.0 - 78.0*sat.theta2 + 137.0*sat.theta4);
			sat.x1m5th = 1.0 - 5.0*sat.theta2;
			sat.omgdot = -0.5*temp1*sat.x1m5th + 0.0625*temp2*(7.0 - 114.0*sat.theta2 +395.0*sat.theta4)
				+ temp3*(3.0 - 36.0*sat.theta2 + 49.0*sat.theta4);
			sat.xhdot1 = -temp1*sat.cosio;
			sat.xnodot = sat.xhdot1 + (0.5*temp2*(4.0 - 19.0*sat.theta2)
				+ 2.0*temp3*(3.0 - 7.0*sat.theta2))*sat.cosio;
			sat.omgcof = sat.bstar*sat.c3*Math.cos(sat.omegao);
			sat.xmcof = -Cte.tothrd*sat.coef*sat.bstar*Cte.ae/sat.eeta;
			sat.xnodcf = 3.5*sat.betao2*sat.xhdot1*sat.c1;
			sat.t2cof = 1.5*sat.c1;
			sat.xlcof = 0.125*sat.a3ovk2*sat.sinio*(3.0 + 5.0*sat.cosio)/(1.0 + sat.cosio);
			sat.aycof = 0.25*sat.a3ovk2*sat.sinio;
			sat.delmo = Math.pow(1.0 + sat.eta*Math.cos(sat.xmo),3.0);
			sat.sinmo = Math.sin(sat.xmo);
			sat.x7thm1 = 7.0*sat.theta2 - 1.0;
			if (!(sat.isimp == 1))
			{
				sat.c1sq = sat.c1*sat.c1;
				sat.d2 = 4.0*sat.aodp*sat.tsi*sat.c1sq;
				temp = sat.d2*sat.tsi*sat.c1/3.0;
				sat.d3 = (17.0*sat.aodp + sat.s4)*temp;
				sat.d4 = 0.5*temp*sat.aodp*sat.tsi*(221.0*sat.aodp + 31.0*sat.s4)*sat.c1;
				sat.t3cof = sat.d2 + 2.0*sat.c1sq;
				sat.t4cof = 0.25*(3.0*sat.d3 + sat.c1*(12.0*sat.d2 + 10.0*sat.c1sq));
				sat.t5cof = 0.2*(3.0*sat.d4 + 12.0*sat.c1*sat.d3 + 6.0*sat.d2*sat.d2 + 15.0*sat.c1sq*(2.0*sat.d2 + sat.c1sq));
			}
  			sat.iflag = 0;
		}
		// Update for secular gravity and atmospheric drag.

		xmdf = sat.xmo + sat.xmdot*tsince;
		omgadf = sat.omegao + sat.omgdot*tsince;
		xnoddf = sat.xnodeo + sat.xnodot*tsince;
		omega = omgadf;
		xmp = xmdf;
		tsq = tsince*tsince;
		xnode = xnoddf + sat.xnodcf*tsq;
		tempa = 1.0 - sat.c1*tsince;
		tempe = sat.bstar*sat.c4*tsince;
		templ = sat.t2cof*tsq;
		if (!(sat.isimp == 1))
		{
			delomg = sat.omgcof*tsince;
			delm = sat.xmcof*(Math.pow(1.0 + sat.eta*Math.cos(xmdf),3.0) - sat.delmo);
			temp = delomg + delm;
			xmp = xmdf + temp;
			omega = omgadf - temp;
			tcube = tsq*tsince;
			tfour = tsince*tcube;
			tempa = tempa - sat.d2*tsq - sat.d3*tcube - sat.d4*tfour;
			tempe = tempe + sat.bstar*sat.c5*(Math.sin(xmp) - sat.sinmo);
			templ = templ + sat.t3cof*tcube + tfour*(sat.t4cof + tsince*sat.t5cof);
		}
		a = sat.aodp*tempa*tempa;
		e = sat.eo - tempe;
		xl = xmp + omega + xnode + sat.xnodp*templ;
		beta = Math.sqrt(1.0 - e*e);
		xn = Cte.xke/Math.pow(a,1.5);

		// Long period periodics
		axn = e*Math.cos(omega);
		temp = 1.0/(a*beta*beta);
		xll = temp*sat.xlcof*axn;
		aynl = temp*sat.aycof;
		xlt = xl + xll;
		ayn = e*Math.sin(omega) + aynl;

		// Solve Kepler's Equation
		capu = FuncionesMatematicas.fmod2p(xlt - xnode);
		i = 1;
		epw = 0;
		do
		{
			i++;

			if (i == 2) // Primera iteración
				temp2 = capu;
			else
				temp2 = epw;
			sinepw = Math.sin(temp2);
			cosepw = Math.cos(temp2);
			temp3 = axn*sinepw;
			temp4 = ayn*cosepw;
			temp5 = axn*cosepw;
			temp6 = ayn*sinepw;
			epw = (capu - temp4 + temp3 - temp2)/(1.0 - temp5 - temp6) + temp2;
		}
		while (!((Math.abs(epw - temp2) <= Cte.e6a)||(i>10)));

		// Short period preliminary quantities

  		ecose = temp5 + temp6;
  		esine = temp3 - temp4;
  		elsq = axn*axn + ayn*ayn;
  		temp = 1.0 - elsq;
  		pl = a*temp;
  		r = a*(1.0 - ecose);
		temp1 = 1.0/r;
		rdot = Cte.xke*Math.sqrt(a)*esine*temp1;
		rfdot = Cte.xke*Math.sqrt(pl)*temp1;
		temp2 = a*temp1;
		betal = Math.sqrt(temp);
		temp3 = 1.0/(1.0 + betal);
		cosu = temp2*(cosepw - axn + ayn*esine*temp3);
		sinu = temp2*(sinepw - ayn - axn*esine*temp3);
		u = FuncionesMatematicas.acTan(sinu,cosu);
		sin2u = 2.0*sinu*cosu;
		cos2u = 2.0*cosu*cosu - 1.0;
		temp = 1.0/pl;
		temp1 = Cte.ck2*temp;
		temp2 = temp1*temp;
		// Update for short periodics
		rk = r*(1.0 - 1.5*temp2*betal*sat.x3thm1) + 0.5*temp1*sat.x1mth2*cos2u;
		uk = u - 0.25*temp2*sat.x7thm1*sin2u;
		xnodek = xnode + 1.5*temp2*sat.cosio*sin2u;
		xinck = sat.xincl + 1.5*temp2*sat.cosio*sat.sinio*cos2u;
		rdotk = rdot - xn*temp1*sat.x1mth2*sin2u;
		rfdotk = rfdot + xn*temp1*(sat.x1mth2*cos2u + 1.5*sat.x3thm1);
		// Orientation vectors
		sinuk = Math.sin(uk);
		cosuk = Math.cos(uk);
		sinik = Math.sin(xinck);
		cosik = Math.cos(xinck);
		sinnok = Math.sin(xnodek);
		cosnok = Math.cos(xnodek);
		xmx = -sinnok*cosik;
		xmy = cosnok*cosik;
		ux = xmx*sinuk + cosnok*cosuk;
		uy = xmy*sinuk + sinnok*cosuk;
		uz = sinik*sinuk;
		vx = xmx*cosuk - cosnok*sinuk;
		vy = xmy*cosuk - sinnok*sinuk;
		vz = sinik*cosuk;

		// Position and velocity
		x = rk*ux;  sat.pos[0] = x;
		y = rk*uy;  sat.pos[1] = y;
		z = rk*uz;  sat.pos[2] = z;
		xdot = rdotk*ux + rfdotk*vx;  sat.vel[0] = xdot;
		ydot = rdotk*uy + rfdotk*vy;  sat.vel[1] = ydot;
		zdot = rdotk*uz + rfdotk*vz;  sat.vel[2] = zdot;

		// Convert sat state
		for (i=0;i<3;i++)
		{
			sat.pos[i] = sat.pos[i]*Cte.xkmper;
			sat.vel[i] = sat.vel[i]*Cte.xkmper/60;
		}
		FuncionesMatematicas.magnitud(sat.pos);
		FuncionesMatematicas.magnitud(sat.vel);

	}


	static void SDP4(Satelite sat)
	{
		double tsince = (sat.tiempoJuliano - sat.julian_epoch) * Cte.xmnpda;
		System.out.println("No implementado todavía");
	}



}