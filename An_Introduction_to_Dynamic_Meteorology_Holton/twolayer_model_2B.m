% MATLAB file:  twolayer_model_2B.m
% This program can be used to investigate transient growth of neutral
% waves in the two-level model of Section 8.2 in  Introduction to 
% Dynamic Meteorology. This version differs from 2A in that a finite
% meridional scale with wavenumber m is included.  The effect is to
% replace k^2 by  K^2 = k^2 +m^2 in the dispersion relation.
%  Streamfunctions for the baroclinic and 
% barotropic wave modes are plotted for a disturbance that is initially
% confined entirely to the upper layer. 
% Zonal wavelength can be varied to examine stability cut-off.
% Geometry is a midlatitude f-plane  (beta neglected in this version).
%  (distances are in units of km).
clear all
close all
cor = 2*7.292e-5*sin(pi/4);
beta = 0;                   % beta effect neglected when this term zero
sigma = 2.0e-6;             % static stability parameter
dp = 50000;		            % pressure interval in Pa
lam2 = cor^2/(sigma*dp^2);
Lx = input('zonal wavelength in km  ');
k = 2*pi/(Lx *1.e3);        % zonal wavenumber in units of 1/ m
m = pi/3.e6;		        % meridional wavenumber in 1/m
%  define the grid points on which fields are computed:
xx = linspace(0,Lx,60);     % 60 gridpoints in x   
yy = linspace( 0,3000,15);  % 15 gridpoints in y
[x,y] = meshgrid(xx,yy);    % Sets matrix for grid system in x and y
K2 = k^2 +m^2;
if K2 < 2*lam2
    disp('wavelength corresponds to unstable mode')
    disp('press ^C to terminate the run') 
    pause
end
mu = sqrt((-2*lam2+K2)/(K2+2*lam2));


Um = 0;			            % mean zonal wind
UT = input(' input basic state thermal wind =  ' );
del = (beta*lam2)^2/(K2^2*(K2+2*lam2)^2) -...
    UT^2*(2*lam2-K2)/(K2+2*lam2);
cp1 = Um - beta*(K2+lam2)/(K2*(K2+2*lam2))+sqrt(del);    % mode 1
cp2 = Um - beta*(K2+lam2)/(K2*(K2+2*lam2))-sqrt(del);    %   mode 2
% NOTE that x and y are in km for convenience in graphing
t=0;
dt =  12*3600;              % time interval in seconds
for j = 1:24
    psiM = 1.e7*(exp(i*k*(x*1.e3-cp1*t))...
        -((1-mu)/(1+mu))*exp(i*k*(x*1.e3-cp2*t))).*sin(m*y*1.e3)-Um*y*1.e3;
    psiT = 1.e7*mu*(exp(i*k*(x*1.e3-cp1*t))...
        +((1-mu)/(1+mu))*exp(i*k*(x*1.e3-cp2*t))).*sin(m*y*1.e3)-UT*y*1.e3;
    t = + dt*j;
    figure(1)
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    % psi1 and psi3 are plotted 
    subplot(2,1,1)
    pcolor(x,y,real(psiM+psiT)), title('250 mb streamfunction  (m^2/s)')
    xlabel('x  (km)'), ylabel('y  (km)')
    shading interp
    colorbar('v')
    hold on;
    subplot(2,1,2)
    pcolor(x,y,real(psiM-psiT)), title('750 mb streamfunction  (m^2/s)')
    xlabel('x  (km)'), ylabel('y  (km)')
    shading interp
    colorbar('v')
    str1 = ['time = ' num2str(t/3600) ' hours'];
    text(500, 1000, str1)
    hold off
    %disp('hit any key to advance by 6 hours')
    %pause
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,1,6)
hold off
% remainder of program shows amplitude variation in time as ratio
% of amplitude at time t to initial amplitude
disp('next plot shows time dependence of disturbance amplitude')
disp('this result is not valid for unstable waves')
ratio = zeros(1,24);
tt = zeros(size(ratio));
for j = 1:24;
    tt(j) = (j-1)*dt;
    phaseb = k*mu*UT*tt(j);
    ratio(j) =  sqrt(1+(sin(phaseb))^2*(1-mu^2)^2/(2*mu^2));
end
figure(2)
plot(tt/3600,ratio),xlabel('time in hours'), ylabel('ratio');
axis([0 tt(24)/3600 1 max(ratio)]);
title('ratio of amplitude to initial amplitude');
disp('to replay animation type:   movie(gcf,M)' )
