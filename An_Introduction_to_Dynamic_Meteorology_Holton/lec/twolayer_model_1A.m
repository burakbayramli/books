% MATLAB file:  twolayer_model_1A.m
% This program can be used to investigate baroclinic instability
% using the two-level model of Section 8.2 in  Introduction to 
% Dynamic Meteorology.  Streamfunctions for the baroclinic and 
% barotropic wave modes are plotted for normal mode solutions.
% Growth rate is plotted for unstable modes.
% Zonal wavelength can be varied to examine stability cut-off
% Geometry is a midlatitude beta=plane.
% (Distances are in units of km).
clear all
close all
% first define the grid points on which fields are computed:
cor = 2*7.292e-5*sin(pi/4);
beta = 2*7.292e-5*cos(pi/4)/6.37e6;     % beta at 45 deg north
sigma = 2.0e-6;                         % static stability parameter
dp = 50000;		                        % pressure interval in Pa
lam2 = cor^2/(sigma*dp^2);
Lx = input('input zonal wavelength in km  ');
x = linspace(0, Lx, 40);     
k = 2*pi/(Lx *1.e3);                    % zonal wavenumber in units of 1/ m

Um = 15;   	                            % mean zonal wind
UT = input(' input basic state thermal wind ' );
del = (beta*lam2)^2/(k^4*(k^2+2*lam2)^2) -...
    UT^2*(2*lam2-k^2)/(k^2+2*lam2);
cp = Um - beta*(k^2+lam2)/(k^2*(k^2+2*lam2)) ...
    +sqrt(del);                         % complex zonal phase speed m/s  

% sample geostrophic wind at 500 hPa  in m/s
% NOTE that x and y are in km and k is in km-1 for convenience in graphing
% the factors of 1000 are to convert the expression to m/s
%
t = 0;
psiM = 1.e7*exp(i*k*(x*1.e3-cp*t));
psiT = psiM*(k^2-2*lam2)*UT/((cp-Um)*(k^2+2*lam2)+beta);
% psiT and psiM are plotted 
figure(1)
subplot(2,1,1), plot(x,real(psiM));
hold on;
title('psiM solid, psiT dashed '),xlabel('x  (km)')
ylabel('streamfunction  (m^2/s)')
axis([0 Lx -2.e7 2.e7]), grid
plot(x,real(psiT),'-.')
hold off
subplot(2,1,2), plot(x,real(psiM+psiT))
hold on;
title('psi1 solid, psi3 dashed '),xlabel('x  (km)')
ylabel('streamfunction  (m^2/s)');
axis([0 Lx -2.e7 2.e7]), grid
plot(x,real(psiM-psiT),'-.')

hold off
% remainder of program shows amplitude growth for unstable modes
disp('next plot shows growth rate and fails if mode has neutral stability')
disp('press any key to continue')
pause
dt = 3600;                              % time increment
amp = zeros(1,41);
t = zeros(size(amp));
for j = 1:41
    t(j) = (j-1)*dt;
    amp(j) = abs(exp(-i*k*cp*t(j)));
end
figure(2)
plot(t/3600,amp),xlabel('time in hours')
ylabel('amplitude/(initial amplitude) ')
axis([0 40 1 amp(41)+1])
title('growth rate of unstable mode')



