% MATLAB file  Animate.m
% Template for animation of (x,y) map for  moving system.
% This shows case with mean wind and waves dependent on y as sin(1.5*k*y).
% First define the grid points on which fields are computed.
% This version has y-dependent wave fields so more realistic.
% (Distances are in units of km).

clear all
close all
xx=linspace(-3000,3000,30);  % 30 gridpoints in x   
yy=linspace( -1000,1000,10);      % 10 gridpoints in y
[x,y]=meshgrid(xx,yy);       % Sets matrix for grid system in x and y
%   *********Define the constants*********
k = 2*pi/6000;          % zonal wavenumber in units of 1/ km
l = 1.5*k;		        % meridional wavenumber in 1/km
c = 25;                 % zonal phase speed m/s
U = 30;                 % mean zonal wind
cor = 1.e-4;            % coriolis parameter
V = 15;                 % wave amplitude
JM = 20;                % number of panels in animation
% Sample geostrophic wind at 500 hPa  in m/s.
% NOTE that x and y are in km and k is in km-1 for graphing.
% The factors of 1000 are to convert the expression to m/s
%
t = 0;
dt = 3600/1000;         % time scaled to match k in km
for j = 1:20  
    %g eopotential distribution
    phi = 55000-U*cor*(y)*1.e3+ V*cor/k*1.e+3*sin(k*(x-c*t)).*cos(l*(y)); 
    t = t+ dt;
    figure(1)
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    % phi is contoured with contour lines labelled
    subplot(2,1,1)
    [cs,h]=contour(x,y,phi/9.8);
    clabel(cs,h,'LabelSpacing',432)
    title('geopotential height (meters)')
    xlabel('x (km)'), ylabel('y (km)')
    axis([-3000,3000,-1000,1000])
    subplot(2,1,2)
    [cs,h]=contour(x,y,phi);
    clabel(cs,h,'LabelSpacing',432)
    title('geopotential (m^2/s^2)')
    xlabel('x (km)'), ylabel('y (km)')
    H = gcf;
    M(:,j) = getframe(H);
end
    movie(H,M)
