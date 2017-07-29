% MATLAB file:  rossby_wave_1.m
% This program illustrates the dependence of phase velocity on 
% zonal wavelength for a single zonal harmonic barotropic Rossby
% wave on a midlatitude beta=plane.
% Contour plot routine gives streamfunction and velocity vector.
% See section 7.7 in Introduction to Dynamic Meteorology.
% This version has y-dependent wave fields  
%  (Distances are in units of km).
close all
clear all
% first define the grid points on which fields are computed:
xx = linspace(-6000,6000,60);       % 60 gridpoints in x   
yy = linspace( 0,6000,15);          % 15 gridpoints in y
[x,y] = meshgrid(xx,yy);            % Sets matrix for grid system in x and y
cor = 2*7.292e-5*sin(pi/4);
beta = 2*7.292e-5*cos(pi/4)/6.37e6; % beta a 45 deg north
vp = 10;                            % perturbation meridional wind
Lx = input('zonal wavelength in km ');    
k = 2*pi/Lx;                        % zonal wavenumber in units of 1/ km
m =  pi/6000;		                % meridional wavenumber in 1/km

U = input('input mean zonal wind in m/s ');

cp = U - beta/(k^2+m^2)*1.e6;       % zonal phase speed m/s  

% Sample geostrophic wind at 500 hPa  in m/s
% NOTE that x and y are in km and k is in km-1 for convenience in graphing
% the factors of 1000 are to convert the expression to m/s.
t = 0;
dt = 6*3600/1000;                   % time scaled to match k in km
for j = 1:48
    psi = cor*(U*1000/m*cos(m*y)+vp*1000/k*sin(k*(x-cp*t)).*sin(m*y));
    zeta = -vp*1000/k*(k^2+m^2)*1.e-6*sin(k*(x-cp*t)).*sin(m*y)-...
        U*m/1000*cos(m*y);
    t = + dt*j;
    set(gca,'NextPlot','replacechildren')
    hold off
    % zeta is contoured  
    str1 = ['time = ' num2str(t/3.6) ' hours'];
    subplot(2,1,1), contour(x,y,psi)
    title('geopotential'), xlabel('x (km)'), ylabel('y (km)')
    text(-1000, 500, str1)
    axis([-6000,6000,0,6000])
    subplot(2,1,2), pcolor(x,y,zeta),title('vorticity')
    xlabel('x (km)'), ylabel('y (km)')
    shading interp
    axis([-6000,6000,0,6000])    %keeps axis same as in top subplot
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,2)
disp('to replay animation type:   movie(gcf,M)' )
