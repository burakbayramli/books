% MATLAB file geost_adjust_2.m
% Geostrophic adjustment in 1-d shallow water model for initial height
% disturbance consisting of a confined surface displacement
% defined by the function     h(x) = -hm/(1+(x/L)^2).
% Student can vary Coriolis parameter and width of initial height disturbance
% to see influence of rotation and scale on the response
% Fast fourier transform is used to determine spectral coefficients for
% solution using ode solver then inverse transforming.

disp('barotropic Rossby adjustment problem')
disp('initial condition  h(x) = -hm/(1+(x/L)^2 ')
clear all
close all
lat = input('give  latitude in degrees ');
cor = 2*7.2921e-5*sin(pi*lat/180);
runtime = input('integration time in days ');
time = runtime*24*3600;
L = input('give zonal scale of initial disturbance in km (200 km minimum) ');
Lx = 16000;                 % Lx is the width of the domain in km 
k = 2*pi/(Lx*1.e3);         % lowest zonal wavenumber in m-1
csq = 20^2;                 % shallow water speed squared csq = gH
alph = 1.e-5;               % damping rate for u component
N = 128;                    % number of modes for Fourier transform
x = linspace(0, Lx, N); 
xm = Lx/2;                  % location of maximum disturbance in km

% set the initial conditions on u(x), v(x), p(x)
p0 = -400./(1+((x-xm)/L).^2);

% Fourier transform p0(x) to get P0(s)
disp('please wait while time integration is performed')
P0 = fft(p0,N);
U0 = zeros(size(P0));
V0 =zeros(size(P0));
options = [];               % place holder
for s = 1: N/2;
    ls = +i*k*(s-1);
    als = alph*(s/N)^2;
    % solution is saved once every two  hours
    [t,y] = ode45('yprim_adj_2',[0:7200:time],[U0(s) V0(s) P0(s)],...
        options,cor,csq,ls,alph);
    U(:,s) = y(:,1);
    V(:,s) = y(:,2);
    P(:,s) = y(:,3);
end 
dt = length(t);
for j = 1:dt;
    u(j,:) = real(ifft(U(j,:),N));
    v(j,:) = real(ifft(V(j,:),N));
    p(j,:) = real(ifft(P(j,:),N));
end 
figure(1)
axis square
set(gca,'NextPlot','replacechildren')
for j = 1:dt 
    subplot(3,1,1),  plot(x,u(j,:)) 
    axis([0 Lx -6 6])
    xlabel('x (km)'), ylabel('u velocity (m/s)')
    str1 = ['time = ' num2str(t(j)/3600) '  hours'];
    title(str1, 'Fontsize',12)
    subplot(3,1,2), plot(x,v(j,:))
    axis([0 Lx -3 3])
    xlabel('x (km)'), ylabel( 'v velocity (m/s)')
    subplot(3,1,3), plot(x,p(j,:))
    axis([0 Lx -400 100])
    xlabel('x (km)'), ylabel('height (m)')
    H= gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,1,8)
disp('to repeat movie enter  command    movie(H,M,2) ')

