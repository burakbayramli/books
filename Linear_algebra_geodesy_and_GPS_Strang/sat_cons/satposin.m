function satp = satposin(t,eph);
%SATPOSIN  Calculation of X,Y,Z coordinates in an INERTIAL
%	        reference frame at time t with given ephemeris eph

%Kai Borre 11-15-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1997/12/06 $

GM = 3.986005e14; % earth's universal gravitational parameter
		            % m^3/s^2

%  Units are either seconds, meters, or radians
%  Assigning the local variables to eph
svprn	   =   eph(1);
af2	   =   eph(2);
M0	      =   eph(3);
roota	   =   eph(4);
deltan	=   eph(5);
ecc	   =   eph(6);
omega	   =   eph(7);
cuc	   =   eph(8);
cus	   =   eph(9);
crc	   =  eph(10);
crs	   =  eph(11);
i0	      =  eph(12);
idot	   =  eph(13);
cic	   =  eph(14);
cis	   =  eph(15);
Omega0	=  eph(16);
Omegadot =  eph(17);
toe	   =  eph(18);
af0	   =  eph(19);
af1	   =  eph(20);
t0c	   =  eph(21);

% Procedure for coordinate calculation
A = roota*roota;
t = check_t(t-t0c);
n0 = sqrt(GM/A^3);
n = n0+deltan;
M = M0+n*t;
M = rem(M,2*pi);
E = M;
E_old = E+.001;
while abs(E-E_old) >= 1.e-12
   E_old = E;
   E = M+ecc*sin(E);
end
v = atan2(sqrt(1-ecc^2)*sin(E), cos(E)-ecc);
phi = v+omega;
phi = rem(phi,2*pi);
u = phi 	            + cuc*cos(2*phi)+cus*sin(2*phi);
r = A*(1-ecc*cos(E)) + crc*cos(2*phi)+crs*sin(2*phi);
i = i0+idot*t	      + cic*cos(2*phi)+cis*sin(2*phi);
Omega = Omega0+Omegadot*t;
x1 = cos(u)*r;
y1 = sin(u)*r;
satp(1,1) = x1*cos(Omega)-y1*cos(i)*sin(Omega);
satp(2,1) = x1*sin(Omega)+y1*cos(i)*cos(Omega);
satp(3,1) = y1*sin(i);
%%%%%%%%% end satposin.m %%%%%%%%%
