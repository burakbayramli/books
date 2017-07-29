% MATLAB file: lee_wave_1.m
% Lee-wave in 1-d stratified model for 
% ridge defined by the function   h(x) = hm/(1+(x/L)^2).
% Student can vary  width of ridge (L)and mean zonal flow U.
% Fast fourier transform is used to determine spectral coefficients
% for the solution. 
clear
close all 
disp('topographic wave for isolated ridge')
disp('ridge profile  h(x) = hm/(1+(x/L)^2 ')
%  Set parameters
bv = 1.e-4;                     %buoyancy frequency squared
g = 9.81;
%
U = input('give mean zonal wind in m/s  ');
Lz = 30;     		             % depth of domain in km
L = 20;    		                 % zonal scale of ridge in km  
Lx = 500;                        % Lx is the length of the domain in km 
k = 2*pi/(Lx*1.e3);              % lowest zonal wavenumber in m
%%*********************************************
% Damping rate 'alph'  can be varied 
disp(  'damping rate per second =  ') 
r = 5.0e-6             %damping rate per second
%%*********************************************
NL = 61;                		% number of vertical gridpoints
N = 128;                        % number of modes for Fourier transform
uxz = zeros(NL,N);
wxz = zeros(NL,N);
zz = linspace(0,Lz,NL);
xx = linspace(0, Lx, N); 
dz = Lz/(NL-1);
s = [1:N];
xm = Lx/2 ;                      % location of ridge in km
hx = 4000./(1+((xx-xm)/L).^2);   % topographic profile
% Fourier transform hx(x) to get hn(s)
hn = fft(hx,N);
hn(1) = 0;
%  solve for streamfunction for each fourier mode
w=zeros(NL,N);
u=zeros(NL,N);
psi=zeros(NL,N);
for n = 2:N
    ns = n-1;
    %  vertical wavenumber squared for analytic solution for each zonal wavenumber
    m2 = bv/U^2-(k*ns)^2;
    %  vertical velocity for each Fourier mode
    wsurf = i*k*U*hn;
    for j=1:NL
        w(j,n) = wsurf(n)*exp(i*sqrt(m2)*zz(j)*1.e3);  %vertical profile of w
        % streamfunction contours (approximate trajectories)
        u(:,n) = -sqrt(m2)*w(:,n)/k; 
        psi(:,n)=i*u(:,n)/sqrt(m2);
    end
end
for j = 1:NL
    uxz(j,:) = real(ifft(u(j,:),N));    % disturbance zonal wind in x,z space
    wxz(j,:) = real(ifft(w(j,:),N));    % vertical velocity
    psixz(j,:) = real(ifft(psi(j,:),N)) -U*j*1.e3*dz;
end
[x, z] = meshgrid(xx,zz); 
figure(1)
[cs H] = contour(x,z,wxz);
clabel(cs,H)
title('vertical velocity (m/s)')
xlabel( 'horizontal distance (km)'), ylabel('height (km)')
figure(2)
[cs H] = contour(x,z,uxz-U);
clabel(cs,H)

title('zonal velocity perturbation (m/s)')
xlabel( 'horizontal distance (km)'), ylabel('height (km)')
figure(3)
[cs H] = contour(x,z,psixz);
clabel(cs,H)
title('streamlines (m^2/s)')
xlabel( 'horizontal distance (km)'), ylabel('height (km)')
