% MATLAB file Name:  linear_grav_wave_1.m
% Linear gravity wave problem for nonhydrostatic incompressible case
% with variable buoyancy frequency squared BV and variable mean wind UB 
% to be specified analytically. Generalizes section 7.4.1 in text.
% Forcing is specified in terms of lower boundary vertical velocity
% for a sinusoidal topography (See Figure 7.10), and waves are stationary
% relative to the ground.
% Students can alter the parameters in BV and UB to see the effect of changes
% in stability and in doppler shifted phase speed.
% Sample shown has strong reduction in stability at 6 km and
% constant doppler shifted phase speed.
% Gridpoint labelling starts with k=1 at z = dz
clear all
close all
%  Define  constants and parameters
NL = 400;                        % number of vertical gridpoints
ztop = 20.E3;                    % top boundary level
NLM = NL -1;
Lx = input('specify zonal wavelength in km '  )
XK = 2.*pi/(Lx*1.e3);            % zonal wavenumber
wbot= 1.0;                       % lower boundary forcing amplitude
ALPH = 1.E-6;                    % damping rate
DZ = ztop/NL;
% 
% Setup grid and define basic state and forcing variables
Z = linspace(DZ,ztop,NL);
DZ2 = DZ^2;
% note that Z=0 and Z=ztop correspond to K = 0 and K = NL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change arguments in tanh functions below to vary BV and UB profiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define mean zonal wind profile (constant for case given)
UB = (35 - 0*tanh((Z-10.e3)/2.e3));
UB0 = 35;                   % lower boundary mean wind
%  define buoyancy frequency squared  profile
BV = 1.e-4*(2.5-1.5*tanh((Z-6.e3)/1.0e3));
BV0 = 4.e-4;                % lower boundary BV
%
%  define damping as imaginary phase speed (test case)
DAMP = ALPH/XK;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Statements below this point should not be changed!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%            
% define Doppler shifted phase speed
CDOP = -UB +i*DAMP;
% compute mean wind shear and curvature
D2UB = zeros(size(CDOP));
for K = 2:NLM
    D2UB(K) = (UB(K+1)-2.*UB(K)+UB(K-1))/DZ2;
end

% compute index of refraction squared
MSQ = BV./CDOP.^2  + D2UB./CDOP -XK^2;
%
%  Upper boundary condition coefficient
Mztop = sqrt(MSQ(NL));
if CDOP(NL) >= 0
    QT = -i*2*Mztop*DZ;
else
    QT = +i*2*Mztop*DZ;
end
% Coefficient for tridiagonal solver
A = DZ2*MSQ -2;
B = zeros(size(A));
A(NL) = A(NL) +QT;
B(1) = B(1) -wbot;
W = zeros(NL,1);
e = ones(NL,1);
% Define the tridiagonal matrix for finite difference equation
% Note that A.' gives the nonconjugate transpose of row vector A
M = spdiags([e A.' e], -1:1, NL,NL);
M(NL,NL-1) = 2;
%
% solve MW = B for W  by matrix inversion
W = M\B.';
wplot =[ wbot W.'];
zplot = [0 Z*1.e-3];
figure(1)
subplot(1,2,1), plot([UB0 UB],zplot)
title('mean wind'), xlabel(' U  (m/s)'), ylabel('height  (km)')
subplot(1,2,2), plot([BV0 BV],zplot)
title('N^2'), xlabel('s^-^2'), ylabel('height (km)')
disp('press any key to continue')
pause
figure(2)
subplot(1,2,1), plot(real(wplot),zplot)
xlabel('m/s'), ylabel('height  (km)');
title('real part of w')
subplot(1,2,2), plot(imag(wplot),zplot)
xlabel('m/s'), ylabel('height  (km)')
title ('imaginary part of w')
disp('press any key to plot momentum flux and divergence')
pause
% compute momentum flux and momentum flux convergence
for K = 2:NLM
    FM(K) = i*0.5/XK*conj(W(K))*(W(K+1)-W(K-1))/(2.*DZ);
end
FM(1) = FM(2);
FM(NL) = FM(NLM);        
fbot = FM(1);
for K = 2:NLM        
    FMCON(K) = -(FM(K+1)-FM(K-1))/(2.*DZ);
end     
FMCON(1) = 0;
FMCON(NL) = FMCON(NLM);
%  compute amplitude and phase of vertical velocity scaled by exp(-z/2H)
figure(3)
subplot(1,2,1), plot([fbot FM],zplot)
title('momentum flux'), xlabel( 'm^2/s^2'), ylabel('height  (km)')
subplot(1,2,2), plot([ 0 real(FMCON)*24*3600],zplot)
title('zonal force'), xlabel('m/s per day'), ylabel('height  (km)')
disp('press any key to plot amplitude and phase of w')
pause
figure(4)
subplot(1,2,1), plot(abs(wplot),zplot)
title('amplitude of w'), xlabel (' m/s'),ylabel('height  (km)')
PHASE =180/pi* atan2(imag(W),real(W));
subplot(1,2,2), plot([0 PHASE'],zplot)
title('phase of maximum w'), xlabel('degrees'), ylabel('height (km)')
%
%
disp('press any key to contour plot vertical velocity')
pause
%
% contour plot of vertical velocity in x,z plane at t=0
xx = linspace(-pi,pi,40);
[x,y] = meshgrid(xx,zplot);
% following statement creates matrix with wplot values at each grid point
[x,wpr] = meshgrid(xx,wplot);
figure(5)
%
w_contour = real(wpr).*cos(x) -imag(wpr).*sin(x);
pcolor(x,y,w_contour)
shading interp
colorbar('v')
title('vertical velocity'), xlabel('phase (radians)')
ylabel('height (km)')




