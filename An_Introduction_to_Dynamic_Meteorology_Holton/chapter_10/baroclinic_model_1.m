% MATLAB file:  baroclinic_model_1.m   for use with Chapter 10.
% Revised 10/28/02  with 2nd order diffusion
% Finite difference model for two-layer system of Chapter 8.
% Model considers wave-zonal interaction for single wave mode.
% Thermal forcing and friction terms are included.
% Uses leapfrog time differencing with forward steps to suppress
% computational mode.
% Routine calls function "stream4.m" to invert vorticity and
% then plots vorticity and streamfunction.
% Note that domain is beta-plane channel periodic in x.
% Meridional velocity zero at northern and southern boundaries.
% Zonal mean zonal wind zero at northern and southern boundaries.
% U0rad may be varied to change the forcing amplitude.
clear all
close all
Lx = 6000;  Ly = 6000;          % channel dimensions in km
Nx = 33; Ny = 31;               % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx = linspace(0,Lx,Nx);         % Nx gridpoints in x for plotting routines 
yy = linspace( 0,Ly,Ny);        % Ny gridpoints in y
% convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);       % Sets matrix for grid system in x and y
k = 2*pi/(Lx*1000) ;             % zonal wavenumber in units of 1/ m
m = pi/(Ly*1000);                % meridional wavenumber
dy = Ly/(Ny-1)*1000;             % grid distance in y
U00rad = input('specify radiative equilibrium thermal wind (e. g., 25 m/s)  ');
U0rad = U00rad*sin(m*yy*1000);   % radiatively determined mean zonal wind
UM0 = U0rad;                     % initial mean zonal wind jet amplitude
UT0 = UM0/2;                     % initial mean thermal wind
UM = UM0;
UT = UT0;
VT = 0;                          % initial mean meridional wind upper level
R = 287;                         % gas constant
cor = 1.e-4;                     % Coriolis parameter
beta = 1.6e-11;                  % beta effect
Ek = 2.e-6;                      % Ekman friction
dy2 = dy^2;
A2 = 1.e6/dy^2;                  % 2nd order diffusion 
alph = 1.e-6;                    % thermal relaxation rate s^-1
lam2 = 3.e-12;                   % 2*lambda^2
K2 = k^2;
% initial condition for streamfunction
K2lam = K2 + lam2;
A = 1.e-7;                       % initial wave streamfunction amplitude
% example given here is for  single sinusoids in x and y
% compute initial vorticity
zetaM0 = +A*(+sin(m*yy*1000));   % computes initial vorticity
zetaM = zetaM0;
zetaT0 = zetaM0;
zetaT = zetaT0;
qT = zetaT;  qT0 = qT;
psiT = -zetaT/(k^2+m^2);
psiM = -zetaM/(k^2+m^2);
psiT0 = psiT;
%%  time integration parameters:
time_end = input('specify ending time in hours (at least 300)');
time_endsec = time_end*3600;     % ending time in seconds
dt = 300;                        % time increment in seconds
dt2 = 2*dt;
t = -dt;                         % initial time minus dt
tsave = 3600*6;                  % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);     % total number of time steps
nsave = fix(time_endsec/tsave);  % number of times saved to output
F0 = zeros(size(yy));            % initializes rhs forcing terms
G0 = F0; 
GM0 = F0;  FM0 = F0;  
% initialize eddy flux terms
fluxM = F0; fluxT = F0;  fluxTT = F0;  

% Begin time looping using leapfrog
for s=1:ntime
    t = t + dt;
    tt = t/tsave;
    if rem(tt,1) ==0;            % forward step no  
        time = t/3600;           % output the time
        % time marching for mean fields
        FM = fluxM +A2*[0 diff(UM,2) 0]...
            -Ek/2*(UM-2*UT);
        GM = fluxT +A2*[0 diff(UT,2) 0]...
            +cor*VT+Ek/2*(UM-2*UT);
        vforce = lam2/cor*(fluxT-[0 diff(fluxTT,2)/dy2 0]+Ek/2*(UM-2*UT)...
            + alph*(UT-U0rad));
        % advance one time step
        UMn = UM+ dt*FM;
        UTn = UT + dt*GM;
        VT = stream4(Nyl,dy,0,lam2,vforce);
        % time marching for the wave fields
        d2UT = [0 diff(UT,2)/dy2 0];
        dqTbary = -d2UT+lam2*UT;
        betaeff = beta-[0 diff(UM,2)/dy2 0];
        F0 = -i*k*(UM.*zetaM+betaeff.*psiM+UT.*zetaT-d2UT.*psiT)-Ek/2*(zetaM-2*zetaT)...
            +A2*[0 diff(zetaM,2) 0];
        G0 = -i*k*(UM.*qT+betaeff.*psiT+UT.*zetaM+dqTbary.*psiM)...
            +Ek/2*(zetaM-2*zetaT)+lam2*alph*psiT0...
            +A2*[0 diff(zetaT,2) 0];
        
        % predict eddy fields at new time
        zetaMn = zetaM +dt*F0;
        qTn = qT +dt*G0;
        psiT0 = psiT;
        psiT = stream4(Nyl,dy,k, lam2,qTn);
        psiM = stream4(Nyl,dy,k, 0,zetaMn);
        qT0 = qT;  qT = qTn;
        zetaM0 = zetaM;  zetaM = zetaMn;
        zetaT0 = zetaT;
        zetaT = qT +lam2*psiT;
        UM0 = UM;  UM = UMn;
        UT0 = UT;  UT = UTn;
        fluxT = k/2*real(i*(psiT.*conj(zetaM)+psiM.*conj(zetaT)));
        fluxM = k/2*real(i*(psiM.*conj(zetaM)+psiT.*conj(zetaT)));
        fluxTT = k/2*real(i*(psiM.*conj(psiT)));
        
        time = t/3600            % show output  time in hours
        figure(1)
        [x,psiplot] = meshgrid(xx*1000,psiT);
        psiplot = real(psiplot.*exp(i*k*x));
        subplot(2,1,1)
        pcolor(x/1000,y/1000,psiplot)
        shading interp
        xlabel('x  (km)'), ylabel('y  (km)')
        title('psiT')
        subplot(2,1,2)
        plot(UT,yy,'k',UM,yy,'b',VT,yy,'r', U0rad,yy,'g')
        title('UT=black, UM = blue, VT = red, Urad = green')
        xlabel('velocity (m/s)'), ylabel( 'y  (km)')
    end                            % end of forward step
    KE(s) = mean(-psiM.*conj(zetaM)-conj(psiM).*zetaM);
    timex(s) = t/3600;
    % leap frog steps 
    vforce = lam2/cor*(fluxT-[0 diff(fluxTT,2)/dy2 0]+Ek/2*(UM0-2*UT0)...
        + alph*(UT0-U0rad));
    VT = stream4(Nyl,dy,0,lam2,vforce);
    FM = fluxM +A2*[0 diff(UM0,2) 0]...
        -Ek/2*(UM0-2*UT0);
    GM = fluxT +A2*[0 diff(UT0,2) 0]...
        +cor*VT+Ek/2*(UM0-2*UT0);
    
    % advance one time step
    UMn = UM0+ dt2*FM;
    UTn = UT0 + dt2*GM;
    % time marching for the wave fields
    d2UT = [0 diff(UT,2)/dy2 0];
    dqTbary = -d2UT+lam2*UT;
    betaeff = beta-[0 diff(UM,2)/dy2 0];
    F0 = -i*k*(UM.*zetaM+betaeff.*psiM+UT.*zetaT-d2UT.*psiT)-Ek/2*(zetaM0-2*zetaT0)...
        +A2*[0 diff(zetaM0,2) 0];
    G0 = -i*k*(UM.*qT+betaeff.*psiT+UT.*zetaM+dqTbary.*psiM)...
        +Ek/2*(zetaM0-2*zetaT0)+lam2*alph*psiT0...
        +A2*[0 diff(zetaT0,2) 0];
    
    % predict eddy fields at new time
    zetaMn = zetaM0 +dt2*F0;
    qTn = qT0 +dt2*G0;
    psiT0 = psiT;
    psiT = stream4(Nyl,dy,k, lam2,qTn);
    psiM = stream4(Nyl,dy,k, 0,zetaMn);
    qT0 = qT;  qT = qTn;
    zetaM0 = zetaM;  zetaM = zetaMn;
    zetaT0 = zetaT;
    zetaT = qT +lam2*psiT;
    UM0 = UM;  UM = UMn;
    UT0 = UT;  UT = UTn;
    fluxT = k/2*real(i*(psiT.*conj(zetaM)+psiM.*conj(zetaT)));
    fluxM = k/2*real(i*(psiM.*conj(zetaM)+psiT.*conj(zetaT)));
    fluxTT = k/2*real(i*(psiM.*conj(psiT)));
    
end
figure(2)
subplot(2,1,1)'
plot(UT,yy,'k',UM,yy,'b',VT,yy,'r', U0rad,yy,'g')
title('UT=black, UM = blue, VT = red, Urad = green')
xlabel('velocity (m/s)'), ylabel( 'y  (km)')
subplot(2,1,2)
plot(abs(zetaM)*1.e5,yy,'b',abs(zetaT)*1.e5,yy,'k')
title('zetaM, blue; zetaT, black')
xlabel('vorticity times 1.e5 (s^-^1)'), ylabel( 'y  (km)')
figure(3)
plot(timex,KE) 
ylabel('kinetic energy  (m^2/s^2)'), xlabel('time  (hours)')
figure(4)
plot(fluxM*1.e5,yy,'k',fluxT*1.e5,yy,'g')
title(':  fluxM=black;  fluxT=green')
xlabel ('vorticity fluxes (m/s^2) times 1.e5'), ylabel('y  (km)')
plot(fluxTT*2*cor/R,yy)
xlabel('meridional heat flux in K m/s '), ylabel('y  (km)');
%%%%%%%%%%%%%%%%%%%%%%%
