% Program 'rocket_drag_mass.m' for calculating the mass and drag of a rocket
% with specified stage data, as a function of time, radius, and
% relative speed.
% (c) 2010 Ashish Tewari
% Requires the standard atmosphere code 'atmosphere.m'.
function [D,m]=rocket_drag_mass(t,r,v)
% r: radius (km)
% v: relative speed (km/s)
% m: mass (kg)
% D: drag (N)
global c; %Nose radius (m) specified in the calling program.
global S; %Base area (m^2) specified in the calling program.
global m01; %I-stage initial mass (kg) specified in the calling program.
global m02; %II-stage initial mass (kg) specified in the calling program.
global mL; %Payload mass (kg) specified in the calling program.
global ms1 %I-stage structural mass (kg) specified in the calling program.
global ms2 %II-stage structural mass (kg) specified in the calling program.
global tb1; %I-stage burn time (s) specified in the calling program.
global tb2; %II-stage burn time (s) specified in the calling program.
global M;
%Mach numbers at which CDc is tabulated in the calling program.
global CDc1; %I-stage CDc values specified in the calling program.
global CDc2; %II-stage CDc values specified in the calling program.
global CDcL; %Payload CDc values specified in the calling program.
global Gamma;%Specific-heat ratio (1.41 for air) specified in the
	     %calling program.

mu=398600.4;
v=v*1000;
alt=(r-6378.14)*1000;
atmosp = atmosphere(alt,v,c);
rho = atmosp(2);
Qinf = 0.5*rho*v^2;
mach = atmosp(3);
Kn=atmosp(4);
s = mach*sqrt(Gamma/2);
CDFM=1.75+sqrt(pi)/(2*s);
if t<=tb1
m=m01-(m01-m02-ms1)*t/tb1;
CDC=interp1(M,CDc1,mach);
elseif t<=(tb1+tb2)
m=m02-(m02-mL-ms2)*(t-tb1)/tb2;
CDC=interp1(M,CDc2,mach);
else
m=mL;
CDC=interp1(M,CDcL,mach);
end
iflow=atmosp(6);
if iflow==2
CD=CDC;
elseif iflow==1
CD=CDFM;
else
CD = CDC + (CDFM - CDC)*(log10(2*Kn)/3+0.5113);
end
D=Qinf*S*CD;
