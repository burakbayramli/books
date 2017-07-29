% MATLAB file:  rossby_3.m
% Topographic rossby wave in 1-d shallow water model for ridge
% defined by the function     h(x) = -hm/(1+(x/L)^2).
% Student can vary Coriolis parameter and width of ridge (L)
% to see the influence of rotation and zonal scale on the response
% Fast fourier transform is used to determine spectral coefficients
% for the solution  
disp('topographic Rossby wave for isolated ridge')
disp('ridge profile  h(x) = -hm/(1+(x/L)^2 ')
clear
close all
lat = 45;                   % central latitude of channel
cor = 2*7.2921e-5*sin(pi*lat/180);
beta = 2*7.2921e-5*cos(pi*lat/180)/6.37e6;
%
U = input('give mean zonal wind in m/s  ');
L = 400;                    % zonal scale of ridge in km  
Lx = 48000;                 % Lx is the length of the domain in km 
H = 8000;		            % mean depth of fluid layer
k = 2*pi/(Lx*1.e3);         % lowest zonal wavenumber in 
m =  pi/(8.0e6); 
%%*********************************************
% Damping rate 'alph'  can be varied 

disp(  'damping rate per second =  ') 
r = 5.e-6             %damping rate per second
%%*********************************************
N = 128;                  % number of modes for Fourier transform
n = 1:N;
ns = n-1;
Kn2 = (k*ns).^2+m^2;
eps(2:N) = r*Kn2(2:N)./(k*ns(2:N)*U);
eps(1) = 0;
xx = linspace(0, Lx, N); 
xm = Lx/2;                % location of maximum disturbance in km
% set the initial conditions on u(x), v(x), p(x)
hx = 2000./(1+((xx-xm)/L).^2);
% Fourier transform hx(x) to get hn(s)

hn = fft(hx,N);
hn(1) = 0;
psin = cor*hn./(H*(Kn2-beta/U-i*eps));
psi = real(ifft(psin,N));
vn = i*(k*ns).*psin;
v = real(ifft(vn,N));
yy= linspace(0,6000,10);
[x,y] = meshgrid(xx,yy);
[psir,y] = meshgrid(psi,yy);
[hxy, y] = meshgrid(hx,yy);
hxy_contour = hxy.*sin(pi*y/6000);
psi_contour = psir.*sin(pi*y/6000)-U*y*1.e3;

subplot(3,1,1),  plot(xx,hx)
axis([0 Lx 0 2000])
ylabel('topography (m)')
subplot(3,1,2), plot(xx,psi*cor/9.8)
axis([0 Lx min(psi*cor/9.8)   max(psi*cor/9.8) ])
ylabel('height field (m)')
subplot(3,1,3), plot(xx,v)
axis([0 Lx min(v)   max(v) ])
xlabel('x in km'); ylabel('v  (m/s) ')
disp('hit any key to continue')
pause
figure(2)
pcolor(x,y,psi_contour)
shading interp
colorbar('v')
title('streamfunction')
hold on
contour(x,y,hxy_contour, [1000 1000])
ylabel('y in km'), xlabel('x in km')



