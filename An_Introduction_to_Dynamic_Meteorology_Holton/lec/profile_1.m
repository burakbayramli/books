% MATLAB script:  profile_1.m
% This script reads in a thermodynamic sounding and plots profiles
% of various thermodynamic parameters.
clear all                   % clear the workspace
close all                   % close all figures
% The data is given on height surfaces
load sounding.dat           % loads z, p, theta, qv  data
z = sounding(1,:);
p = sounding(2,:);
theta = sounding(3,:);
qv = sounding(4,:);
% Define physical constants
dz = 250;
g = 9.81;                   % gravity
R = 287;                    % gas constant for dry air
cp = 1004;                  % specific heat at constant press
Ttr = 273.16;               % triple point temperature
Lv = 2.5e6;                 % latent heat of vaporization
estr = 6.11;                % saturation vapor pressure at triple point (hPa)
Rv = 461;                   %  gas constant for vapor
eps = 0.622;                % ratio of water vapor mass to dry air mass
% Plot potential temperature versus pressure 
s=size(theta);
% Note by using the size function the code will work for data files with
% different numbers of points.  
% Compute  temperature  profile.  
T = theta.*(p/1000).^(R/cp); %compute temperature from theta
% Compute  vapor pressure, saturation vapor pressure, and mixing ratio.
esat= estr*exp(Lv/Rv*(1/Ttr-1./T));     % saturation vapor pressure
eair = qv.*p/eps;                       % vapor pressure for actual humidity
qvs = eps*esat./p;                      % saturation mixing ratio
% Compute T when parcels at each level are brought to saturation.
% (Use Clausius-Clapeyrn equation)
Tpsat = 1./(1/Ttr-Rv/Lv*log(eair./estr));
figure(1)
plot(T,p,Tpsat,p)       % plot temperature and saturation temperature.
axis ij                 % this statement reverses y-axis order
axis([150 300 100 1000])
xlabel('Temperature  (K)'), ylabel('pressure  (hPa)')
title('temperature and saturation temperature')
figure(2)
plot(qv,p, qvs,p)
axis ij
xlabel('mixing ratio')
axis([0 .025 100 1000])
ylabel('pressure  (hPa)')
title(' water vapor mixing ratio and saturation mixing ratio ')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Student to do the computations below
%compute saturation equivalent potential temperature from eq. (9.40)
thetae = theta;     %%%dummy value used
%compute equivalent potential temperature. 
thetaes = theta;    %%%  dummy value used
figure(3)
plot(theta,p, thetaes, p, thetae, p) 
axis ij
xlabel('potential temperature (K)')
ylabel('pressure  (hPa)')
axis([300 400 100 1000]);
title(' theta,  thetae,  thetaes')
nl = s(2);       %number of vertical levels
%need to compute  lifting condensation level and level of free convection
% plfc is  pressure at level of free convection
% Tmlfc is saturation temperature at level of free convection
% Tmoist is function to compute moist adiabat through LCL
% lcl is lifting condensation level for parcel raised from lowest level 
% Compute lcl by lifting dry adiabatically from level 1 until saturated
lcl = min(find(theta(1)*(p/1e3).^(R/cp)<Tpsat(1)));
Tmlcl = theta(1)*(p(lcl)/1e3)^(R/cp);
Tm= Tmoist(p,z, nl, z(lcl), p(lcl),Tmlcl );  %moist adiabat through lcl
% find LFC for lifting from level 1
locate = find((thetae(1)-thetaes)>=0);
lfc = min(locate);
figure(4)
plot(T,p,Tm,p)
axis([190 300 100 1000])
axis ij
xlabel('temperature  (K)'), ylabel('pressure  (hPa)')
title('temperature and moist adiabat')
%Student to compute CAPE for the sounding 
