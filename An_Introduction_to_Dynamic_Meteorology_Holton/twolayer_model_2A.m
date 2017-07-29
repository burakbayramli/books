% MATLAB file:  twolayer_model_2A.m
% This program can be used to investigate transient growth of neutral
% waves in the two-level model of Section 8.2 in  Introduction to 
% Dynamic Meteorology.  Streamfunctions for the baroclinic and 
% barotropic wave modes are plotted for a disturbance that is initially
% confined entirely to the upper layer. 
% zonal wavelength can be varied to examine stability cut-off.
% Geometry is a midlatitude f-plane or beta plane.
% (Distances are in units of km).
clear all
close all
% first define the grid points on which fields are computed:
cor = 2*7.292e-5*sin(pi/4);
beta = 0.e-11;              % beta effect neglected when this term zero
sigma = 2.0e-6;             % static stability parameter
dp = 50000;		            % pressure interval in Pa
lam2 = cor^2/(sigma*dp^2);
Lx = input('input a zonal wavelength in km  ');
x = linspace(0, Lx, 40);     
k = 2*pi/(Lx *1.e3);         % zonal wavenumber in units of 1/ m
if k^2 < 2*lam2;
    disp('wavelength is too large for neutral mode')
    disp('press ^C (control and C) to terminate the run') 
    pause
end
mu = sqrt((-2*lam2+k^2)/(k^2+2*lam2));


Um = 0;		                % mean zonal wind
UT = input(' input basic state thermal wind =  ' );
del = (beta*lam2)^2/(k^4*(k^2+2*lam2)^2) -...
    UT^2*(2*lam2-k^2)/(k^2+2*lam2);
cp1 = Um - beta*(k^2+lam2)/(k^2*(k^2+lam2)) ...
    +sqrt(del);             % pos root complex zonal phase speed m/s  
cp2 = Um - beta*(k^2+lam2)/(k^2*(k^2+lam2)) ...
    -sqrt(del);             % neg root complex zonal phase speed m/s  

% NOTE that x and y are in km for convenience in graphing
t=0;
dt = 6*3600;                % time interval in seconds
for j = 1:24
    psiM = 1.e7*exp(i*k*(x*1.e3-cp1*t))...
        -1.e7*((1-mu)/(1+mu))*exp(i*k*(x*1.e3-cp2*t));
    psiT = 1.e7*mu*exp(i*k*(x*1.e3-cp1*t))...
        +1.e7*mu*((1-mu)/(1+mu))*exp(i*k*(x*1.e3-cp2*t));
    t = + dt*j;
    figure(1)
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    % psiT and psiM are plotted 
    subplot(2,1,1), plot(x,real(psiM));
    hold on
    title('psiM solid, psiT dashed '), xlabel('x  (km)')
    ylabel('streamfunction  (m^2/s)')
    axis([0 Lx -2.e7 2.e7]), grid
    plot(x,real(psiT),'-.')
    hold off
    subplot(2,1,2), plot(x,real(psiM+psiT))
    hold on
    title('psi1 solid, psi3 dashed '),xlabel('x  (km)')
    ylabel('streamfunction  (m^2/s')
    axis([0 Lx -2.e7 2.e7]), grid
    str1 = ['time = ' num2str(t/3600) ' hours'];
    text(300, -1.e7, str1)
    plot(x,real(psiM-psiT),'-.')
    hold off
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,1,6)
hold off
% remainder of program shows amplitude variation in time as ratio
% of amplitude at time t to initial amplitude
disp('next plot shows time dependence of disturbance amplitude')
ratio = zeros(1,24);
tt = zeros(size(ratio));
for j = 1:24
    tt(j) = (j-1)*dt;
    phaseb = k*mu*UT*tt(j);
    ratio(j) =  sqrt(1+(sin(phaseb))^2*(1-mu^2)^2/(2*mu^2));
end
figure(2)
plot(tt/3600,ratio), xlabel('time in hours'), ylabel('ratio')
axis([0 tt(24)/3600 1 max(ratio)])
title('ratio of amplitude to initial amplitude')
disp('to replay animation type:   movie(gcf,M)' )
