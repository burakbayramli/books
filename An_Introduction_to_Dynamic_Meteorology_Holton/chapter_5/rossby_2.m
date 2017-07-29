% MATLAB: file  rossby_wave_2.m
% This illustrates dispersion of free barotropic Rossby waves 
% on a midlatitude beta plane.
% Starts with an initially compact height disturbance given by
% the function  h(x) = -hm/(1+(x/L)^2).
% Contour plot routine gives streamfunction.  
% See section 7.7 in Introduction to Dynamic Meteorology
% This version has y-dependent wave fields  
% Fast fourier transform is used to determine spectral coefficients for
% solution  in x.  (Distances are in units of km).
%
clear all
close all
lat = 45;                    % central latitude of channel
cor = 2*7.2921e-5*sin(pi*lat/180);
beta = 2*7.2921e-5*cos(pi*lat/180)/6.37e6;
r = 2.e-6;                   % damping rate per second
L = 500;                     % zonal scale of initial disturbance in km  
Lx = 60000;                  % Lx is the width of the domain in km 
k = 2*pi/(Lx*1.e3);          % lowest zonal wavenumber in m
m = pi/(6.0e6);              % meridional wavenumber in m

N = 256;                    % number of modes for Fourier transform
n = 1:N;
s = n-1;
ks = k*s;
Ks2 = ks.^2+m^2;
freq = -beta*ks./Ks2-i*r;
% first define the grid points on which fields are computed:
xx = linspace(0, Lx, N); 
yy = linspace(0,6000,10);
[x,y] = meshgrid(xx,yy);
xm = Lx/2 ;                 % location of maximum disturbance in km
% set the initial conditions on h(x)
hx0 = 200./(1+((xx-xm)/L).^2);
% Fourier transform hx(x) to get hn(s)
hs0 = fft(hx0,N);
hs0(1) = 0;                 % subtract out the zonal mean part of height field
% time step to compute dispersion of initial disturbance
%
t = 0;
time = input('number of days for simulation  ');

figure(1)
dt = 12*3600;                % time step of 1/2 day (in seconds)
for j = 1:time*2
    hs = hs0.*exp(-i*freq*t);
    hx = real(ifft(hs,N));
    t = dt*j;
    [hxy, y] = meshgrid(hx,yy);
    hxy = hxy.*sin(pi*y/6000);
    set(gca,'NextPlot','replacechildren')
    hold off
    str1 = ['time = ' num2str(t/24/3600) ' days'];
    subplot(2,1,1), plot(xx,hx)
    axis([0 Lx -40 40])
    title('geopotential height at channel center (m)')
    text(25000, -30, str1)
    ylabel('height  (m)')
    
    % height field is contoured  
    subplot(2,1,2), pcolor(x,y,hxy)
    caxis([-8 8])
    title('geopotential height topopgraphy')
    xlabel('x  (km)'), ylabel('y (km)')
    shading interp
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,1,4)
disp(' to replay animation slowly type:  movie(gcf,M,2,1)')                                                                                                             