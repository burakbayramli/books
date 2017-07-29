% MATLAB File Name:  linear_grav_wave_3.m
% Linear gravity wave problem: hydrostatic case for stratified atmosphere
% with variable buoyancy frequency squared BV and variable mean wind UB
% to be specified analytically (for stratospheric waves).
% Includes animation of wave vertical velocity field.
% Students can alter the parameters in BV and UB to see the effect of changes
% in  stability and in doppler shifted phase speed.
% Sample shown has tropopause at 10 km and strong reduction in doppler shifted
% phase speed near 25.
% Gridpoint labelling starts with k=1 at z = dz
clear
close all
% Define  constants and parameters
NL = 200;                       % number of vertical gridpoints
ZTOP = 40.E3;                   % top boundary level
CPHASE = 38;                    % zonal phase speed
H = 7.4E3;                      % scale height
NLM = NL-1;
DH = ZTOP;                      % top boundary of heating
XK = 2*pi/(1.E6);               % zonal wavenumber
WBOT = 0.1;                     % lower boundary forcing amplitude
ALPH = 1.E-6;                   % linear damping rate
DZ = ZTOP/NL;                   % grid increment
% 
% setup grid and define basic state and forcing variables
Z = linspace(DZ,ZTOP,NL);
DZ2 = DZ^2;
RHO = exp(-Z/H);
rho2 = exp(+Z/(2*H));
% note that Z=0 and Z=ztop correspond to K = 0 and K = NL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change arguments in tanh functions below to vary BV and UB profiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define mean zonal wind profile
UB = (18 + 18*tanh((Z-25.e3)/4.e3));
UB0 = 0;                        % lower boundary zonal wind
%  define buoyancy frequency squared  profile
BV = 1.e-4*(2.5+1.5*tanh((Z-10.e3)/1.0e3));
BV0 = 1.e-4;                    % lower boundary buoycancy freq. squared
%
% define damping as imaginary phase speed (test case)
DAMP = ALPH/XK;
% define Doppler shifted phase speed
CDOP = CPHASE -UB +i*DAMP;
%   compute mean wind shear and curvature
DUB = zeros(size(CDOP));
D2UB = DUB;
FM = DUB;
FMCON = DUB;
B = DUB;
for K = 2:NLM
    DUB(K) = (UB(K+1) -UB(K-1))/(2.*DZ);
    D2UB(K) = (UB(K+1)-2.*UB(K)+UB(K-1))/DZ2;
end
%          
% compute index of refraction squared
MSQ = BV./CDOP.^2 -0.25/H^2 +( DUB/H + D2UB)./CDOP;
%
% Upper boundary condition coefficient
% radiation condition requires sign of doppler shifted phase speed
MZTOP = sqrt(MSQ(NL));
if CDOP(NL) >= 0
    QT = -i*2*MZTOP*DZ;
else
    QT = +i*2*MZTOP*DZ;
end          
% Coefficient for tridiagonal solver
A = DZ2*MSQ -2.;
A(NL) = A(NL) +QT;
B(1) = B(1) -WBOT;
W = zeros(NL,1);
e = ones(NL,1);
% Define the tridiagonal matrix for finite difference equation
% Note that A.' gives the nonconjugate transpose of row vector A
M = spdiags([e A.' e], -1:1, NL,NL);
M(NL,NL-1) = 2;
%
% solve MW = B for W  by matrix left division
W = M\B.';
wvert = rho2.*W.';        % vertical velocity
% for plotting add the z=0 level
wplot =[ WBOT wvert];    
zplot = [0 Z*1.e-3];
figure(1)
subplot(1,2,1), plot([UB0 UB],zplot)
title('mean wind'), xlabel('m/s'), ylabel('height (km)')
subplot(1,2,2), plot([BV0 BV],zplot)
title('N^2'), xlabel('s^-^2'), ylabel('height (km)')
disp('press any key to plot real and imaginary w_prime')
pause
figure(2)
plot(real(wplot),zplot,imag(wplot),zplot)
xlabel('w in m/s'), ylabel('height (km)')
title('real (blue) and imaginary (green) parts of w')
disp('press any key to plot momentum flux and divergence')
pause
% Compute momentum flux and momentum flux convergence
for K = 2:NLM
    FM(K) = real(i*0.5/XK*conj(W(K))*((W(K+1)-W(K-1))/(2.*DZ)...
        +W(K)/(2.*H)));
end
FM(1) = FM(2);
FM(NL) = FM(NLM);        
fbot = FM(1);
for K = 2:NLM        
    FMCON(K) = -(FM(K+1)-FM(K-1))/(2.*DZ)/RHO(K);
end     
FMCON(NL) = FMCON(NLM);
% Compute amplitude and phase of vertical velocity scaled by exp(-z/2H)
figure(3)
subplot(1,2,1), plot([fbot FM],zplot)
title('momentum flux'), xlabel( 'kg/m/s^2'), ylabel('height (km)')
subplot(1,2,2), plot([ 0 real(FMCON)*24*3600],zplot)
title('zonal force'), xlabel('m/s per day'), ylabel('height (km)')
disp('press any key to plot amplitude and phase of w')
pause
figure(4)
subplot(1,2,1), plot(abs(wplot),zplot)
title('amplitude of w'), xlabel (' m/s'),ylabel('height (km)')
PHASE =180./pi* atan2(imag(W),real(W));
subplot(1,2,2), plot([0 PHASE'],zplot)
title('phase of maximum w'), xlabel('degrees'), ylabel('height (km)')
%
%
disp('press any key to animate a contour plot of vertical velocity')
pause
%
% Contour plot of vertical velocity in x,z plane animated
xx = linspace(-pi,pi,40);
[x,y] = meshgrid(xx,zplot);
% Following statement creates matrix with wplot values at each grid point
% Here x is nondimensionalized with zonal wavenumber xk to display one wavelength
[x,wpr] = meshgrid(xx,wplot);
figure(5)
%
t=0;
dt = 1800;
M = moviein(30);
for j=1:30
    t =  +dt*j;
    w_contour = real(wpr).*cos(x-XK*CPHASE*t) -...
        imag(wpr).*sin(x-XK*CPHASE*t);
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    pcolor(x,y,w_contour)
    shading interp
    colorbar('v')
    str1 = ['time = ' num2str(t/3600) '  hours'];
    text(-1, 38, str1, 'Fontsize',12)
    title('vertical velocity (m/s)')
    xlabel('kx  (radians)'), ylabel('height (km)')
    H= gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,2)





