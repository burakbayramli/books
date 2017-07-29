% MATLAB file:  pv_anomaly.m
% Script to plot geopotential tendency for specified upper level
% quasi-geostrophic pv anomaly advection
% pv advection is uniform for p<250 hPa and zero for p>250 hPa
% used to derive figure 6.10 for 4th edition of Intr. Dyn. Meteor.
% Shows downward influence of upper level pv advection by
% solution for eq. (6.26).
% Solution normalized to uniform height tendency at p = 0 hPa.
% NOTE:  script uses function  suptitle.m  for plotting title.
clear all 
close all
xx=linspace(0,3000,40);         % 30 gridpoints in x   
pp=linspace(0,100000,81);       % 81 gridpoints in pressure
[x,p]=meshgrid(xx,pp);          % Sets matrix for grid system in x and z
%   *********Define the function to be contoured*********
% NOTE that x is in km and k is in km-1 for convenience in graphing
% the factors of 1000 are to convert the expression to geopotential meters
% pressure is given in Pascals, converted to hPa for plots
cor = 1.e-4;                    % Coriolis parameter
Q0 = 1.e-5;                     % pv anomaly  magnitude
U = 50;                         % Advecting zonal wind
sig = 2.e-6;                    % static stability parameter
Lx = 2000 ;                     % zonal wavelength in km
m = 0*(pi/6.e6);                % meridional wavenumber
k = 2*pi/(Lx*1000);             % zonal wavenumber
lam2 = (k^2+m^2)*sig/cor^2; 
lam = sqrt(lam2);
ps = 1.e5;                      % surface pressure

X00= -sig*Q0*U*(k)/(cor*lam2)*24*3600;  % units of (m/s)^2  per day

X0 = X00*sin(k*x*1000);
X = zeros(size(p));
z1 = lam*ps/4; z2=lam*3*ps/4; z3 = lam*ps;
for n=1:81
    if pp(n)>ps/4
        X(n,:) = X0(n,:).*sinh(z1)/sinh(z3).*cosh(lam*(p(n,:)-ps))/9.8;
    end
    if pp(n)<=ps/4
        X(n,:) = X0(n,:).*(1-sinh(z2)/sinh(z3).*cosh(lam*p(n,:)))/9.8;
    end
end
Xmin = min(X(5,:));
Xnorm = X/Xmin;
figure(1)
subplot (1,2,1)
[cs, h]=contour(x,p/100,Xnorm,[ .2 .4 .6 .8]);
xlabel ('x (km)')
axis([0 1000 100 1000])
ylabel('pressure (hPa)')
title('Lx = 2000 km ')
axis ij
clabel(cs,h,'manual')
Lx = 6000;                  % zonal wavelength
k = 2*pi/(Lx*1000);
lam2 = (k^2+m^2)*sig/cor^2;
lam = sqrt(lam2);

X00 = -sig*Q0*U*(k)/(cor*lam2)*24*3600;  %units of (m/s)^2  per day

X0 = X00*sin(k*x*1000);
X=zeros(size(p));
z1=lam*ps/4; z2=lam*3*ps/4; z3 = lam*ps;
for n=1:81
    if pp(n) > ps/4
        X(n,:) = X0(n,:).*sinh(z1)/sinh(z3).*cosh(lam*(p(n,:)-ps))/9.8;
    end
    if pp(n) <= ps/4
        X(n,:)  = X0(n,:).*(1-sinh(z2)/sinh(z3).*cosh(lam*p(n,:)))/9.8;
    end
end
Xmin = min(X(5,:));
Xnorm = X/Xmin;
subplot(1,2,2)
[cs,h]=contour(x,p/100,Xnorm,[.2 .4 .6 .8]);
axis([0 3000 100 1000])
xlabel('x (km)')
axis ij
title('Lx = 6000 km')
clabel(cs,h,'manual')
suptitle('normalized height tendency')



