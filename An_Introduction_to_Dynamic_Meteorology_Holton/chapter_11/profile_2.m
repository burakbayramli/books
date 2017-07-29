% MATLAB script:  profile_2.m
% This script shows simple example of reading in data and plotting 
% monthly mean  profiles versus pressure for mixing ratio and
% saturation mixing ratio, also computes and plots  potential temperature
% and equivalent potential temperature.
% Note that data file to be loaded must consist entirely of columns of
% data.
% In the example there are 3 columns corresponding to pressure, 
% temperature, and number of soundings respectively. each column has M rows
% A(3,M) is the matrix containing the data
% The data is given on pressure surfaces
% Hypsometric equation used to get data on geopotential surfaces
clear all                   % clear the workspace
close all                   % close all figures
A = load('tropical_temp.dat');  %load the data
%Define physical constants
g = 9.81;                   % gravity
R = 287;                    % gas constant for dry air
cp = 1004;                  % specific heat at constant press
Ttr = 273.16;               % triple point temperature
Lv = 2.5e6;                 % latent heat of vaporization
estr = 6.11;                % saturation vapor pressure at triple point (hPa)
Rv = 461;                   % gas constant for vapor
eps = 0.622;                % ratio of water vapor mass to dry air mass
                % plot temperature versus pressure 
                % first define an altitude vector with M columns:
s = size(A);    % s is a row vector giving size of A
% Note by using the size function the code will work for data files with
% different numbers of points.  
% Statement below gives z with same number of rows as A.

z(1) = 0;                   % sea level
T0 = 300;                   % surface temperature
p0 = 1010;                  % surface pressure
p = [p0 A(:,1)']; 
T = [T0 A(:,2)'];
for n=1:s(1)                % compute geopotential height
    z(n+1) =z(n)+R/(2*g)*(T(n+1)+T(n))*log(p(n)/p(n+1));
end
hum = 0.75*ones(size(T));   % relative humidity is 75%
hum(1:4) = 0.90;            % boundary layer humidity is 90%
for n = 31:s(1)+1
    hum(n) = 0.1 ;          % stratospheric humidityis 10%
end
% modify tocompute potential temperature  profile
% compute saturation vapor pressure, vapor pressure and mixing ratio
esat = estr*exp(Lv/Rv*(1/Ttr-1./T));        % saturation vapor pressure
eair = hum.*esat;                           % vapor pressure for actual humidity
qv = eps*eair./p;                           % mixing ratio
qvs = eps*esat./p;                          % saturation mixing ratio
Tpsat = 1./(1/Ttr-Rv/Lv*log(eair./estr));   % T for parcel brought to saturation
figure(1)
plot(T,p,Tpsat,p)                   % plot temperature and saturation temperature.
axis ij                             % this statement reverses y-axis order
axis([150 300 100 1000])
xlabel('Temperature (K)'), ylabel('pressure  (hPa)')
title('temperature (blue) and saturation temperature (green)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Student to compute and plot theta, thetae, thetaes
% CAPE, and parcel vertical velocity profiles with no entrainment
% and with entrainment factor of lambda = 1.e-4