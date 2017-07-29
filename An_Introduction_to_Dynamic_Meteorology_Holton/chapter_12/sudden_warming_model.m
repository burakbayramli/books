% MATLAB file:  sudden_warming_model.m
% Quasi-geostrophic channel model for wave mean-flow interaction.
% Based on  Holton and Mass (1976) J. Atmos. Sci.
% Uses 3rd order Adam-Bashforth time defferencing.
% Forcing is specified in terms of lower boundary geopotential disturbance.
% Waves are stationary and have single meridional mode for this example.
% Wave disturbance psi0 specified at lower boundary at z = 16 km.
% This version includes rayleigh friction acting on mean wind
% and newtonian relaxation on mean wind and waves.
% Radiation condition on waves at top boundary.
% Gridpoint labelling starts with j=1 at z = dz= 18 km.
% Beta plane channel is centered at 60N with 60 degree width.
clear all
close all
% define  constants and parameters
nl = 46;                        % number of vertical gridpoints
ztop = 90.E3;                   % top boundary level (relative to 16 km)
nlm = nl -1;
rad = 6.37e6;                   % radius of earth
s = input('specify planetary wavenumber, (e. g., 1, 2, or 3)   s = ');
k = s/(rad*cos(pi/3));          % zonal wavenumber
l = 3/rad;                      % meridional wavenumber
hb= input('specify lower boundary height disturbance,(e. g., 200 m )   hb = ');
% setup grid and define basic state and forcing variables
time_end = input('specify integration time (at least 90 days), time_end =   ');
time_endsec = time_end*24*3600; % ending time in seconds
dz = ztop/nlm;
ubtime = zeros(nl,time_end);    % time series for mean wind
psitime = zeros(nl,time_end);   % time series for wave streamfunction
day = linspace(1,time_end,time_end);
z = linspace(dz, ztop, nlm);    % vertical prediction levels
sh = 7000;		                % density scale height
rho = exp(-z/sh);               % basic state density profile
rho2 = sqrt(rho);               % square root density
dt = 3600;                      % time step increment in seconds
dt12 = dt/12;
t = 0;                          % initial time
psi = zeros(size(z));           % initial wave field
alph = (1.5+tanh((z-19000)/7000))*1.e-6;      % radiative damping rate
dalph = 1.e-6*(sech((z-19000)/7000)).^2/7000; % d(alph)/dz
gamma = (1.+tanh((z-40000)/15000))*1.e-6; 
% 
dz2 = dz^2;
% Note that z=0 and z=ztop correspond to j = 0 and j = nlm
% upper boundary condition on psi is applied at z = ztop +dz.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change arguments in  functions below to vary bv and ubrad  profiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define radiative equilibrium mean zonal wind profile, 
% ubrad, and the initial mean zonal wind ub.
ubrad0 = 0;
ubrad = ubrad0 + 2.e-3*z;
ub0 = 15;                         % lower boundary zonal wind
ub = ub0 + 45*cos(pi*(z*1.e-3-45)/90);
ubref = ub;
%  define buoyancy frequency squared  profile
bv = 4.e-4;		                  % stratospheric stability
FKn = zeros(size(z));             % vector for wave forcing at time n
FKn1 = FKn;                       % vector for wave forcing at time n-1
FKn2 = FKn;                       % vector for wave forcing at time n-2
FAn = FKn;                        % vector for mean forcing at time n
FAn1 = FKn;                       % vector for mean forcing at time n-1
FAn2 = FKn;                       % vector for mean forcing at time n-2
%%%%%%%   define other parameters 
cor = 1.46e-4*sin(pi/3);          % coriolis parameter for 60N
beta = 1.46e-4*cos(pi/3)/rad;     % beta effect
cobv = cor^2/bv;                  %  f^2/N^2
dzlc = dz2*l^2/cobv;
p = dalph./alph;                 
eps = 8/(3*pi);			          % weighting function  
K1 = bv*(k^2 + l^2)/cor^2;
lkeps  = l^2*k*eps/2;
sig = 2 +dz2/(4*sh^2)-p*dz2/(2*sh);
tau = 2.5e5;                      % lower boundary wave turn on time
%
%   Coefficient for tridiagonal solver
e = ones(nlm,1);
% Define the time independent tridiagonal matrices for wave and mean
% finite difference equations
%
Mk = 1 + p*dz/2;
Nk = 1-p*dz/2;
ujm = 1 + dz/(2*sh);
ujp = 1 - dz/(2*sh);
Mu = ujp+p*dz/2;
Nu = ujm -p*dz/2;
A1 = spdiags([ujm*e -2*e ujp*e], -1:1, nlm,nlm);            % for defining betae
A1(nlm,nlm) = A1(nlm,nlm) +ujp;
A2 = zeros(size(z));
Au = spdiags([ujm*e -(2+dzlc)*e  ujp* e], -1:1, nlm,nlm);   % for mean wind eq.
Au1 = spdiags([Nu'.*e -2*e  Mu'.* e], -1:1, nlm,nlm);       % for mean wind eq.
Au(nlm,nlm) = Au(nlm,nlm) +ujp;
Au1(nlm,nlm) = Au1(nlm,nlm) +Mu(nlm);
Au2 = zeros(size(z));
Au2(1) = Nu(1)*(ub0-ubrad0);
Au2(nlm) = -Mu(nlm)*(ubrad(nlm)-ubrad(nlm-1));
Dupsi = spdiags([e -2*e e], -1:1, nlm,nlm);
Dupsi2 = zeros(size(z));
%
% time integration by Adams-Bashforth 3rd order differencing
ntime = fix(time_endsec/dt);
% Begin time stepping
%%%%%%   Define betae at each time step
for n = 1:ntime
    A2(1) = ujm*ub0;           % sets dub/dz at lower boundary
    dub = A1*ub';
    betae = beta + eps*l^2*ub -cobv*eps/dz2*(dub' +A2);
    %%%%%%   Time stepping for the wave equation
    %
    QK = 1/(4*sh^2) +K1;
    B = dz2*QK;
    % define upper boundary radiation condition
    m2 = -QK + betae(nlm)/(cobv*eps*ub(nlm));
    imdz = i*2*dz*sqrt(m2);
    S1 = spdiags([Nk'.*e -sig'.*e Mk'.*e], -1:1, nlm,nlm);
    S1(nlm,nlm) = -sig(nlm) + imdz*Mk(nlm);  % modified upper boundary coefficient
    S1(nlm,nlm-1) = 2;
    Dupsi(nlm,nlm) = -(2 +imdz);             % modified upper boundary matrix
    %%%% Note that +imdz is needed in Dupsi because it operates on psi conjugate %%
    Dupsi(nlm,nlm-1) = 2;                    % modified upper boundary matrix
    S2  = zeros(size(z));                    % vector for  lower boundary condition
    psi0m = 9.8*hb/cor*(1-exp(-(t+dt)/tau));
    psi0 = 9.8*hb/cor*(1-exp(-t/tau));
    S2(1) = Nk(1)*psi0;
    MA = spdiags([e -(2+B.').*e  e], -1:1, nlm,nlm);
    MA(nlm,nlm) = -(2 +B) +imdz;
    MA(nlm,nlm-1) = 2;
    MA1 = zeros(size(z));
    MA2 = zeros(size(z));
    MA1(1) = (psi0 -psi0m);
    MA2(1) = psi0;
    FKn = (-i*k*(eps*ub'.*(MA*psi.'+MA2')+(dz2/cobv*betae.*psi).') - ...
        alph'.*(S1*psi.'+ S2')).'; 
    psimn = psi +(MA\(MA1.' + dt12*(23*FKn.' -16*FKn1.' +5*FKn2.'))).';
    FKn2 = FKn1;
    FKn1 = FKn;
    %  Next 2 lines define EP flux convergence for mean wind equation
    Dupsi2(1) = 9.8*hb/cor*(1-exp(-(t)/tau));
    Cu = lkeps./rho'.*imag(psi.'.*(Dupsi*psi'+Dupsi2'));
    psi = psimn;              %updates the  psi field
    %%%%%%%%%%%%  end of wave equation time step
    %%%%%%    time stepping for mean wind equation
    FAn = gamma*dzlc.*(ub)-alph.*(Au1*(ub-ubrad)'+Au2')' +Cu';
    ubm = ub+ dt12*(Au\(23*FAn' -16*FAn1' +5*FAn2'))';
    ub = ubm;                %updates the ub field
    FAn2 = FAn1;
    FAn1 = FAn;
    %%%%%%  end of mean wind time step
    t = t + dt;
    tt = t/(24*3600);
    if rem(tt,1) == 0;
        ubplot = [ub0 ub];
        psiplot = [psi0 abs(psi)./rho2]*cor/9.8;
        ubtime(:,tt) = ubplot';
        psitime(:,tt) = psiplot';
    end	
end;
disp('press any key to continue')
zplot = [16 16+z*1.e-3];
figure(1)
plot([ub0 ub],zplot,[ub0 ubref],zplot,'--k');
xlabel('m/s'), ylabel('height km')
title('initial ubar (dashed); final ubar (solid)')
pause
psiplot =[psi0 psi./rho2];
figure(2)
subplot(1,2,1), plot(abs(psiplot),zplot)
title('amplitude of psi'), xlabel(' m^2/s'), ylabel('height km');
PHASE = 180./pi* atan2(imag(psi),real(psi));
subplot(1,2,2), plot([0 PHASE],zplot)
title('phase of maximum psi'), xlabel('degrees'), ylabel('height km')
pause
%
%
%
%contour plot of stream function in x,z plane
xx = linspace(-pi,pi,40);
[x,yy] = meshgrid(xx,zplot);
% following statement creates matrix with psiplot values at each grid point
[x,psipr] = meshgrid(xx,psiplot);
figure(3)
%
psi_contour = real(psipr).*cos(x) -imag(psipr).*sin(x);
pcolor(x,yy,psi_contour)
xlabel('longitude'), ylabel('height (km)')
shading interp
colorbar('v')
title('stream funcition'),xlabel('kx (radians)'),ylabel('height km')
pause
figure(4)
plot(day,ubtime(10,:),day,ubtime(20,:),'--k')
xlabel('time (days)'), ylabel('velocity (m/s)')
title('ubar at 20 km (solid) and 40 km (dashed) in m/s')
[daygrd,yy] = meshgrid(day,zplot);
pause
figure(5)
subplot(2,1,1)
pcolor(daygrd,yy,ubtime)
ylabel('height (km)')
title('mean zonal wind (m/s)')
shading interp
hold on
contour(daygrd,yy,ubtime,15,'k')
colorbar('v')
subplot(2,1,2)
pcolor(daygrd,yy,psitime)
ylabel('height (km)'), xlabel( 'time (days)')
title('wave geopotential height (m)')
shading interp
hold on
contour(daygrd,yy,psitime,15,'k')
colorbar('v')

