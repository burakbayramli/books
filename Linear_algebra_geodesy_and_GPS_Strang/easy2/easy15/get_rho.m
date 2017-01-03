function [rho,X_ECEF] = get_rho(tR_RAW,pseudorange,EPH_col,X_receiver)
%GET_RHO  Calculation of distance in ECEF system between
%	      satellite and receiver at time tR_RAW given the
%	      ephemeris Eph.

%Kai Borre 04-01-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 2006/05/21  $

global EPH

% Initial assigment of constants
vlight = 299792458;	         % vacuum speed of light in m/s
Omegae = 7.292115147e-5;	 % rotation rate of the earth in rad/s

%signal sended time=signal received time-signal transmission time
tx_RAW = tR_RAW-pseudorange/vlight;

%time correction acording to system time
toc = EPH(21,EPH_col);
      dt = check_t(tx_RAW-toc);
      tcorr = (EPH(2,EPH_col)*dt + EPH(20,EPH_col))*dt + EPH(19,EPH_col);
      tx_GPS = tx_RAW-tcorr;
      dt = check_t(tx_GPS-toc);
      tcorr = (EPH(2,EPH_col)*dt + EPH(20,EPH_col))*dt + EPH(19,EPH_col);
      tx_GPS = tx_RAW-tcorr; 

% satellite position 
X = satposRTK(tx_GPS,EPH_col);

%geometric range
rho = norm(X-X_receiver(1:3));

%satellite coordinates into inertial coordinate system
omegatau = Omegae*rho/vlight;
R3 = [ cos(omegatau) sin(omegatau) 0;
      -sin(omegatau) cos(omegatau) 0;
              0	       0	     1];
X_ECEF = R3*X;

%geometric range acording to inertial coordinate system
rho = norm(X_ECEF-X_receiver(1:3));
%%%%%%%%% end get_rho.m %%%%%%%%%
