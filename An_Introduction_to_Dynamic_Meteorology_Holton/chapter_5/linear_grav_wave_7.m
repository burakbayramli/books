% MATLAB File Name:  linear_grav_wave_7.m
% Analytic solution for monochromatic oscillating heat source. 
% Linear gravity wave problem for nonhydrostatic incompressible case
% with constant buoyancy frequency squared (bv) and zero mean wind.  
% This version uses surf for 3-d plotting.
% Forcing is specified in terms of heat source with spectral distribution
% in x corresponding to a Gaussian distribution of width L.
% Example shown has heat source with a dependence sin(pi*z/h) for z< h, 
% and heating of 0 for z>h.
% Gridpoint labelling starts with k=1 at z = dz.
clear all
close all
%  define  constants and parameters
nl = 81;                    % number of vertical gridpoints
nlm = nl-1;
nlt = nlm/5;                % depth of heat source
ztop = 20;                  %! top boundary level in km
%
dz = ztop/nlm;
Lh = 10;                    % zonal scale of heat source in km  
wbot = 0;                   % lower boundary vertical velocity
S = 128;                    % number of Fourier modes computed
Lx = 300;                   % width of computational domain

% 
% setup grid and define basic state and forcing variables
zz = linspace(0,ztop,nl);
xx = linspace(0, Lx, S); 
[x,z] = meshgrid(xx,zz);
xm = Lx/2;
bv = 1.e-4;                 % buoyancy frequency squared 

% set the x-dependence of heat source qx
qx = exp(-(xx-xm).^2/Lh^2);
qk =  fft(qx,S);            % Fourier transform qx  to get qk 
qk(1) = 0;                  % subtract out the zonal mean part of heating

% define the z-dependence of heat source  J0
J0 = zeros(size(zz));
for j = 1:nlt 
    J0(j) = sin(pi*zz(j)/4);
end
nu = sqrt(bv)/5;            % oscillation frequency (about 50 minute period)
%
s=linspace(0,S-1,S);

k = 2*pi*s/(1.e3*Lx) ;       % zonal wavenumber for mode s
cphase(2:S)=+nu./k(2:S) ;    % zonal phase speed relative to ground            
cphase(1) = .000001;         % to correct for division by zero for k = 0 mode

% compute index of refraction squared
msq = bv./cphase.^2   -k.^2;
%
ms = sqrt(msq);              % Vertical wavenumber
pih = pi/(4000);
mph = ms.^2 -pih^2;
mh = ms*4000;

%    
G = 1.e-3./cphase.^2.*qk;   % Coefficient for particular solution
w = zeros(size(G)); 
for j = 1:nlt
    weast  = G.*(pih./ms.*exp(-i*mh).*sin(ms.*z(j)*1.e3) + ...
        sin(pih.*z(j)*1.e3))./mph;
    wwest  = G.*(pih./ms.*exp(+i*mh).*sin(ms.*z(j)*1.e3) + ...
        sin(pih.*z(j)*1.e3))./mph;
    wpxe(j,:) = ifft(weast,S);
    wpxw(j,:) = ifft(wwest,S);
end
for j = nlt+1:nl;
    weast = G.*(pih./ms).*sin(mh).*exp(-i*ms.*z(j)*1.e3)./mph;
    wpxe(j,:) = ifft(weast,S);
    wwest = G.*(pih./ms).*sin(mh).*exp(+i*ms.*z(j)*1.e3)./mph;
    wpxw(j,:) = ifft(wwest,S);
end
figure(1)
% time integration
t = 0;
dt = 600;                   %t ime in seconds

for ntime = 1:36;
    w_contour = real(wpxe.*exp(-i*nu*t)+wpxw.*exp(+i*nu*t));
    t = + dt*ntime;
    set(gca,'NextPlot','replacechildren')
    surf(x,w_contour,z)
    axis([0 400 -12 12 0 20])
    shading interp
    title('animation of gravity waves forced by periodic heating')
    xlabel('x (km)'),zlabel('height (km)');
    ylabel('vertical velocity (m/s)')
    H = gcf;
    M(:,ntime) = getframe(H);
end
movie(H,M)
disp(' to replay animation type:  movie(gcf,M,2,6)')             