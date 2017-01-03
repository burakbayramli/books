function [rho,X_ECEF] = get_rho(tR_RAW,pseudorange,Eph,X_receiver)
%GET_RHO  Calculation of distance in ECEF system between
%     	 satellite and receiver at time tR_RAW given the
%	       the pertinent ephemeris Eph.

%Kai Borre 04-01-96
%Copyright (c) by Kai Borre
%$Revision 1.0 $  $Date: 1997/09/23  $

% Initial assigment of constants
vlight = 299792458;	 % vacuum speed of light in m/s
Omegae = 7.292115147e-5;	 % rotation rate of the earth in rad/s

tx_RAW = tR_RAW-pseudorange/vlight;
time = tx_RAW-Eph(21);	 % toc =Eph(21)
%	   af2=Eph(2)	 af1=Eph(20)	 af0=Eph(19)
sat_dt = (Eph(2)*time + Eph(20))*time + Eph(19);
tx_GPS = tx_RAW-sat_dt;
X = satpos(tx_GPS, Eph);
rho = norm(X-X_receiver);
omegatau = Omegae*rho/vlight;
R3 = [ cos(omegatau) sin(omegatau) 0;
	   -sin(omegatau) cos(omegatau) 0;
	          0	          0         1];
X_ECEF = R3*X;
rho = norm(X_ECEF-X_receiver);
%%%%%%%%% end get_rho.m %%%%%%%%%
