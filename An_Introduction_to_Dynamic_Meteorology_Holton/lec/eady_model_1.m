% MATLAB file:  eady_model_1.m
% Contour plots for eady model for maximum baroclinic instability.
% Shows results for most unstable mode for beta = 0 but nonzero m.
% This version includes quiver plot of the Q vector for linearized model.
clear all 
close all
cor = 2*7.292e-5*sin(pi/4);     % Coriolis parameter
bv2 = 1.e-4;                    % buoyancy frequency squared
sigma = 2.0e-6;                 % static stability parameter
Lx = 5500;                      % zonal wavelength in km
k = 2*pi/(Lx*1000);
m = pi/3.e6;		            % meridional wavenumber in 1/m
alph2 = (k^2 + m^2)*bv2/cor^2;
alph = sqrt(alph2);
H = 10000;                      % tropopause height in meters
R = 287;                        % gas constant
Shear = 30/H;                   % shear in sec^-1
%  define the grid points on which fields are computed:
xx = linspace(0,1.5*Lx,31);     % 20 gridpoints in x   
yy = linspace( -1500,1500,21);  % 15 gridpoints in y
zz = linspace(0,H,21);          % 10 gridpoints in z
[x,y] = meshgrid(xx*1000,yy*1000);    % Sets matrix for grid system in x and y
[x,z] = meshgrid(xx*1000,zz);         % matrix for grid system in x and z
% Define coefficients for the model
A = 2.e7;                       % streamfunction amplitude
alphH = alph*H;
c = Shear*H/2*(1+sqrt(1-4*cosh(alphH)/(alphH*sinh(alphH))+4/(alphH^2)));
ci = imag(c);
B = -c*alph*A/Shear;
 
% eddy streamfunction at t = 0
psi = (A*sinh(alph*z)+B*cosh(alph*z)).*exp(i*k*x);  
% eddy vertical velocity
w = (cor/bv2*k*i*((c-Shear*z).*(alph*A*cosh(alph*z)+B*alph*sinh(alph*z))...
    +Shear*(A*sinh(alph*z)+B*cosh(alph*z))).*exp(i*k*x));
% eddy temperature
T = cor*H/R*(alph*A*cosh(alph*z)+alph*B*sinh(alph*z)).*exp(i*k*x);  
psimax = max(max(real(psi)));
V = [0:psimax/4:psimax];
V1 = [-psimax:psimax/4:0];
omax = 1.1*max(max(real(w)));
V2 = [0:omax/4:omax];
V3 = [-omax:omax/4:0];
Tmax = 1.1*max(max(real(T)));
V4 = [0:Tmax/4:Tmax];
V5 = [-Tmax:Tmax/4:0];
figure(1)
subplot(3,1,1)
contour(x/1000,z/1000,real(psi),V,'k')      % x, z, converted to km for graph
hold on
contour(x/1000,z/1000,real(psi),V1,'--k')   % dashed lines for negative contours
pcolor(x/1000,z/1000,real(psi))
shading interp
colorbar('v')
title('streamfunction (m^2/s)'), ylabel('z  (km)')
subplot(3,1,2), contour(x/1000,z/1000,real(w),V2,'k')
hold on
contour(x/1000,z/1000,real(w),V3,'--k')     % dashed lines for negative contours
pcolor(x/1000,z/1000,real(w))
shading interp
colorbar('v')
title('vertical velocity (m/s)'), ylabel('z (km)')

subplot(3,1,3), contour(x/1000,z/1000,real(T),V4,'k');
hold on
contour(x/1000,z/1000,real(T),V5,'--k')
pcolor(x/1000,z/1000,real(T))
shading interp
colorbar('v')
title('Perturbation Temperature (K)')

xlabel('x  (km)'), ylabel('z  (km)')
%plotting of meridional distribution at top and bottom
figure(2)
[psitop,y] = meshgrid(real(psi(21,:)),yy*1000);
[psibot,y] = meshgrid(real(psi(1,:)),yy*1000);
[psimean,y] = meshgrid(real(psi(11,:)),yy*1000);
[Ty,y] = meshgrid(real(T(11,:)),yy*1000);
[wy,y] = meshgrid(real(w(11,:)),yy*1000);
psitop = -Shear*H*y+cos(m*y).*psitop;
psibot = cos(m*y).*psibot;
psimean = -Shear*H*y/2+cos(m*y).*psimean;
Ty = -cor*H/R*Shear.*y +cos(m*y).*Ty; 
wy = cos(m*y).*wy ;

Q1 = 0*sin(m*y);
Q2 = 0*cos(m*y);
omax = max(max(real(psimean))) ;
V = [-omax:omax/4:omax];
omax = 1.1*max(max(real(psibot))) ;
V2 = [0:omax/4:omax];
V3 = [-omax:omax/4:0];
wmax = 1.1*max(max(real(wy))) ;
V4 = [0:wmax/4:wmax];
V5 = [-wmax:wmax/4:0];
subplot(3,1,1)
contour(x/1000,y/1000,psimean,V,'k')    % x, y, converted to km for graph
hold on
pcolor(x/1000,y/1000,Ty)
title('streamfunction (contours)  temperature (color)at z=H/2')
ylabel('y   (km)')
shading interp
colorbar('v')
subplot(3,1,2), contour(x/1000,y/1000,wy,V4,'k');
hold on
contour(x/1000,y/1000,wy,V5,'k--')
pcolor(x/1000,y/1000,Ty)
title('w (contours: solid positive, dashed negative) temperature (color) at z = H/2') 
ylabel('y   (km)')
shading interp
colorbar('v')
subplot(3,1,3)
omax = max(max(real(psitop)));
V = [-omax:omax/4:omax];
contour(x/1000,y/1000,psitop,V,'k')     % x, y, converted to km for graph
hold on
pcolor(x/1000,y/1000,psibot)
xlabel('x   (km)'), ylabel('y    (km)')
title('streamfunction at z = 0 (color) and z= H (contours)')

xlabel('x  (km)'), ylabel('y  (km)')
shading interp
colorbar('v')
hold off


