%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ephemeris calculation                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
format long;

%  Constants
mu = 3.986005e14;									% universal gravitational param (m^3/s^2)
OMEGA_dot_e = 7.2921151467e-5;				% earth's rotation rate (rad/sec)
c = 2.99792458e8;									% speed of light (m/s)
dtr = pi/180;										% convert degrees to radians

%  Given ephemeris data
a_f2 = 0;											% (s/s^2)
a_f1 = -3.183231e-12;							% (s/s)
a_f0 = -2.502208e-4;								% (s)
delta_n = 4.908419e-9;							% (rad/s)
M_0 = -3.682930e-1;								% (rad)
e = 1.521692e-2;									% (unitless)
A = (5.153688e3)^2;								% (m)
t_oe = 252000;										% (s)
C_ic = 6.146729e-8;								% (rad)
C_rc = 2.259375e2;								% (rad)
C_is = 2.086163e-7;								% (rad)
C_rs = 7.321875e1;								% (rad)
C_uc = 4.017726e-6;								% (rad)
C_us = 7.698312e-6;								% (rad)
OMEGA_0 = 1.384688;								% (rad)
omega = -2.466272;								% (rad)
i_0 = 9.464037e-1;								% (rad)
OMEGA_dot = -8.464281e-9;						% (rad/s)
I_dot = 9.178953e-11;							% (rad/s)

%  Time calculations:
%      The notes state that when transmit time is taken into account, 
%      t_sv = 248721.9229  This value is used to calculate GPS system
%      time below.  Note: delta_tr is assumed to be negligible (calculated
%      value of delta_tr = 2.6e-8 sec).
	F = -2*sqrt(mu)/c^2;							% (s/m^1/2)
	t_sv = 248721.9229;
%  t_sv =GPS sec.into the week - Pseudo range L1/speed of light
%  GPS sec.into the week from RGEA log minus transmission time 
	% Estimate delta_tr using t = t_sv
	n_0 = sqrt(mu/A^3);							% (rad/s)
	t_k=t_sv-t_oe;									% Time from eph ref epoch (s)
	n = n_0 + delta_n;							% Corrected mean motion (rad/s)
	M_k=M_0+n*t_k;									% Mean anomaly (rad/s)

	%  Perform Newton-Raphson solution for E_k estimate

	NRnext=0;
	NR=1;
	m=1;
	while abs(NRnext-NR)>1e-15;
   	NR=NRnext;
   	f=NR-e*sin(NR)-M_k;
   	f1=1-e*cos(NR);
   	f2=e*sin(NR);
   	NRnext=NR-(f/(f1-(f2*f/2*f1)));
   	m=m+1;
	end;

	E_k=NRnext;												% Eccentric anomaly ESTIMATE for
																% computing delta_tr
		
delta_tr = F*e*sqrt(A)*sin(E_k);	
delta_t_sv = a_f0 + a_f1*(t_sv-t_oe) + delta_tr;
t = t_sv - delta_t_sv;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Begin ephemeris calculations

n_0 = sqrt(mu/A^3);										% (rad/s)
t_k=t-t_oe;													% Time from eph ref epoch (s)
n = n_0 + delta_n;										% Corrected mean motion (rad/s)
M_k=M_0+n*t_k;												% Mean anomaly (rad/s)

%  Perform Newton-Raphson solution for E_k

NRnext=0;
NR=1;
m=1;
while abs(NRnext-NR)>1e-15;
   NR=NRnext;
   f=NR-e*sin(NR)-M_k;
   f1=1-e*cos(NR);
   f2=e*sin(NR);
   NRnext=NR-(f/(f1-(f2*f/2*f1)));
   m=m+1;
end;

E_k=NRnext;													% Eccentric anomaly (rad)


num=(sqrt(1-e^2)*sin(E_k))/(1-e*cos(E_k));
denom=(cos(E_k)-e)/(1-e*cos(E_k));
v_k=atan2(num,denom);									% True anom (rad)
E_k=acos((e+cos(v_k))/(1+e*cos(v_k)));				% Eccentric anomaly
PHI_k=v_k+omega;											% Argument of latitude 
%
% Second Harmonic Perturbations
%
deltau_k=C_us*sin(2*PHI_k)+C_uc*cos(2*PHI_k);	% Argument of Lat correction
deltar_k=C_rs*sin(2*PHI_k)+C_rc*cos(2*PHI_k);	% Radius correction
deltai_k=C_is*sin(2*PHI_k)+C_ic*cos(2*PHI_k);	% Inclination correction
%
u_k=PHI_k+deltau_k;										% Corr. arg of lat
r_k=A*(1-e*cos(E_k))+deltar_k;						% Corrected radius
i_k=i_0+deltai_k+I_dot*t_k;							% Corrected inclination
%
% Positons in orbital plane
%
xprime_k=r_k*cos(u_k);
yprime_k=r_k*sin(u_k);

OMEGA_k=OMEGA_0+(OMEGA_dot-OMEGA_dot_e)*t_k-OMEGA_dot_e*t_oe;

% ECEF coordinates

x_k=xprime_k*cos(OMEGA_k)-yprime_k*cos(i_k)*sin(OMEGA_k);
y_k=xprime_k*sin(OMEGA_k)+yprime_k*cos(i_k)*cos(OMEGA_k);
z_k=yprime_k*sin(i_k);

% Output Results
fprintf('\nEphemeris Results:\n\n');
fprintf('%9s %22.11f\n','A',A);
fprintf('%9s %22.11f\n','n_0',n_0);
fprintf('%9s %22.11f\n','E_k',E_k);
fprintf('%9s %22.11f\n','delta_tsv',delta_t_sv);
fprintf('%9s %22.11f\n','n',n);
fprintf('%9s %22.11f\n','M_k',M_k);
fprintf('%9s %22.11f\n','v_k',v_k);
fprintf('%9s %22.11f\n','PHI_k',PHI_k);
fprintf('%9s %22.11f\n','delta_uk',deltau_k);
fprintf('%9s %22.11f\n','delta_rk',deltar_k);
fprintf('%9s %22.11f\n','delta_ik',deltai_k);
fprintf('%9s %22.11f\n','u_k',u_k);
fprintf('%9s %22.11f\n','r_k',r_k);
fprintf('%9s %22.11f\n','i_k',i_k);
fprintf('%9s %22.11f\n','xprime_k',xprime_k);
fprintf('%9s %22.11f\n','yprime_k',yprime_k);
fprintf('%9s %22.11f\n','OMEGA_k',OMEGA_k);
fprintf('\n%9s %22.11f\n','x_k',x_k);
fprintf('%9s %22.11f\n','y_k',y_k);
fprintf('%9s %22.11f\n','z_k',z_k);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  

