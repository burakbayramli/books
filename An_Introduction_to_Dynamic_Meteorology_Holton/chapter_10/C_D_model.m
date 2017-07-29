% MATLAB file:  C_D_model.m
% Charney Devore model [JAS 36, p1207, (1979)]
% Calculates vacillation and multiple flow equilibria in truncacted
% barotropic model with topographic wave forcing and "thermal"
% forcing of the zonal flow. One zonal wavenumber and 2 meridional
% wavenumbers are retained total components = 6.
% Vectors  zf, zinit, and z are 6 component vectors with thermal forcing, 
% initial conditions, and solution, respectively for streamfunction 
% components A, K, L, M, N as given in Charney-Devore paper.
% Equations are nondimensionalized with f^-1 for time and L for x and y
% function zprim integrates equation using ode4.
clear all
close all
disp('Charney Devore Model')
N  = 2;                     % zonal wavenumber
lam2 = 0;                   % value for (fL/gh)^2 
K = 0.01;                   % relaxation parameter K
time = 3000;                % run for ~200 days
cor=1.e-4;                  % Coriolis parameter
beta = 1./4;                % beta parameter
h0H = .1;                   % nondimensional topographic height (h0/2H)
zf = zeros(6,1);            % Array for thermal forcing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  zf(1) may be varied from  0.1 to 0.5 to show varied responses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
zf(1) = 0.50;               % forcing of mode 1 zonal flow
zf(2) = 0.0;                % forcing of mode 1 wave
zf(4) = 0.00;               % forcing of mode 2 zonal flow
zf(5) = 0.00;               % forcing of mode 2 wave
options = odeset('RelTol', 1.e-6);
figure(1)
%vector zinit gives amplitudes for initial conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%  zinit(1) can be specified much less than zf(1) or equal to zf(1)
%%%%%% This will determine which stable equilibrium is reached, and in some
%%%%%  cases there will be an oscillatory oscillation depending on initial
%%%%%% conditions.  If zinit(4), zinit(5), zinit(6) all zero 2nd meridional
%%%%%% mode solution is zero always.  A small initial amplitude in
%%%%%% zinit(5) will amplify if 2nd mode is unstable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
zinit(1) = .02;             % Initial amplitude of mode 1 zonal flow
zinit(2) = .0;              % Initial amplitude of mode 1 sine wave
zinit(3) = .0;              % Initial amplitude of mode 1 cosine wave
zinit(4) = .0;              % Initial amplitude of mode 2 zonal wind
zinit(5) = .001;            % Initial amplitude of mode 2 sine wave
zinit(6) = .0;              % Initial amplitude of mode 2 cosine wave
[t,z] = ode45('zprim',[0:1:time],zinit,options,N,lam2,beta,K,h0H,zf);
% plotting time series for all modes
figure(1)
hold on;
subplot(2,1,1), plot(t,z(:,1),'k',t,z(:,2),'r',t,z(:,3),'b');
xlabel('nondimensional time'), ylabel('psi values')
title('first meridional mode')
subplot(2,1,2), plot(t,z(:,4),'k',t,z(:,5),'r',t,z(:,6),'b')
xlabel('nondimensional time'), ylabel('psi values')
title('second meridional mode')

figure(2)     % phase plane plot of first mode against 2nd for zonal mean
plot(z(2000:3000,1),z(2000:3000,4))
xlabel('amplitude of first zonal mean meridional mode')
ylabel('amplitude of second zonal mean meridional mode')
title('phase plane of psiA versus psiC')
% this portion shows maps of the total streamfunction

L= 1500;                        % channel dimensions in km
Nx = 65; Ny = 33 ;              % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx = linspace(0,2*pi*L,Nx);     % Nx gridpoints in x   
yy = linspace( 0,pi*L,Ny);      % Ny gridpoints in y
% convert distances to meters for calculations
[x,y] = meshgrid(xx,yy);        % Sets matrix for grid system in x and y
% define x,y dependence for each coefficient of Z
F1 = sqrt(2)*cos(y/L); F2=2*cos(N*x/L).*sin(y/L); F3=2*sin(N*x/L).*sin(y/L);
F4 = sqrt(2)*cos(2*y/L); F5=2*cos(N*x/L).*sin(2*y/L);
F6 = 2*sin(N*x/L).*sin(y/L);
% compute time series of dimensinional zonal mean zonal wind
for t = 2000:20:3000
    umean= +cor*L*1.e3*(z(t,1)*sqrt(2)*sin(yy/L)+z(t,4)*2*sqrt(2)*sin(2*yy/L));
    figure(3)
    plot(umean,yy/1000)
    title('profiles every 20th time step for steps 2000 to 3000')
    xlabel(' zonal mean velocity (m/s) '), ylabel ('y  in 1000s of km')
    hold on
end
hold off

% plot for last 1000 steps of the integration
j=0;
for t=2500:10:3000
    j=1+j;
    str = num2str(t/10) ;       %  show output  time
    str1 = ['time in hours =' str];
    psi=z(t,1)*F1+z(t,2)*F2+z(t,3)*F3+z(t,4)*F4+z(t,5)*F5+z(t,6)*F6; 
    if j==1 
        omax = max(max((psi)));
        omin = min(min((psi)));
        V = [omin:omax/6:omax];
        topo = h0H*F2;
        omax1 = max(max(abs(topo)));
        V2 = [0:omax1/6:omax1];
        V3 = [-omax1:omax1/4:0];
    end
    figure(4)
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    contour(x/1000,y/1000,psi,[-zf(1):zf(1)/10:zf(1)],'k'); % x, y, converted to km for graph
    xlabel('x  (km)'), ylabel('y  (km)')
    hold on
    pcolor(x/1000,y/1000,topo)
    colormap(pink(5))
    shading interp
    hold off
    xlabel('x'), ylabel('y')
    title('streamfunction contoured, topography colored (red: positive, white negative)')
    text(.25,.5,str1)
    hold off
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,1,4)
disp('to repeat movie twice type:  movie(H,M,2,4)')