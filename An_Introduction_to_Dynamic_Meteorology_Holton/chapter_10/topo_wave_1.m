% MATLAB file:  topo_wave_1.m   for use with Chapter 10
% Finite difference model for linearized barotropic vorticity equation.
% Uses isolated 2-d topography to excite Rossby waves in 2-d for
% beta plane channel with linear vorticity damping.
% Given a vorticity pattern, computes the streamfunction by
% inversion of the Laplacian using Fourier transform in x and FD in y.
% Routine calls function "stream1.m" to invert vorticity and
% then plots vorticity and streamfunction.
% Note that domain is periodic in x.
% Velocity zero at northern and southern boundaries.
clear all
close all
Lx = 24000;  Ly = Lx;               % channel dimensions in km
Nx = 65; Ny = 65 ;                  % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx = linspace(-Lx/4,3*Lx/4,Nx);     % Nx gridpoints in x   
yy = linspace( -Ly/2,Ly/2,Ny);      % Ny gridpoints in y
% convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy = Ly/(Ny-1)*1000;                % grid distance in y
cor = 1.e-4;                        % Coriolis parameter
beta = 1.62e-11;                    % beta at 45 latitude
damp = 4.e-6;                       % damping parameter
U0 = input(' enter a mean zonal wind in m/s  ');
H = 10;                              % fluid depth in km
h0 = 2;                            % mountain peak in km
Lm = 1000*1.e3;                     % mountain scale in  m
htop = h0*Lm^2./(Lm^2+x.^2+ y.^2);  % mountain profile
dhtop = -2*x.*htop./(H*(Lm^2+x.^2+ y.^2));  % d(mountain hgt)/dx

% initial condition for streamfunction (zonally symmetric)

psi = +U0*(Ly*1000 -y);
max(max(psi));
% compute initial vorticity
zeta0 = zeros(size(psi));           % computes initial vorticity
u = U0*ones(size(psi));             % u  
v = zeros(size(psi));               % v = +d(psi)/dx
%%  time integration parameters:
nsv = 0;                            % counter for output figures
t=0;
time_end = input('specify ending time of integration in hours time_end =   ');
time_endsec = time_end*3600   ;     % ending time in seconds
dt = 1800;                          % time increment in seconds
tsave = 12*dt ;     % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);        % total number of time steps
nsave = fix(time_endsec/tsave);     % number of times saved to output

% take forward step as first step
zeta = zeta0 - dt*cor*U0*dhtop;
% call to function to compute stream function at new time from vorticity
psi = stream1(Nxl,Nyl,Lx,dy,zeta);
psi=psi+U0*(Ly*1000 -y);
[dxzeta,dyzeta]=grad(zeta,dx,dy);   % gradient zeta for advection terms
[dxpsi,dypsi] = grad(psi,dx,dy);    % computes gradient of psi                      
v=dxpsi;                            % v = +d(psi)/dx
% Begin time looping using leapfrog method
for s=1:ntime
    t = t + dt;
    tt = t/tsave;
    if rem(tt,1) == 0;    % forward step to damp computational mode
        zetan(:,1:Nxl) = zeta(:,1:Nxl)*(1-damp*dt)...
            - dt*(u(:,1:Nxl).*dxzeta(:,1:Nxl)+v(:,1:Nxl).*(...
            beta)+cor*U0*dhtop(:,1:Nxl));
    else 
        zetan(:,1:Nxl) = zeta0(:,1:Nxl)*(1-damp*dt) ...
            -2*dt*(u(:,1:Nxl).*dxzeta(:,1:Nxl)+v(:,1:Nxl)*(...
            +beta)+cor*U0*dhtop(:,1:Nxl));
    end
    zetan(:,Nx)=zetan(:,1);
    zeta0 = zeta;                         % update old time
    zeta = zetan;
    % call to function to compute  stream function at new time from vorticity
    psi = stream1(Nxl,Nyl,Lx,dy,zeta);
    psi = psi+U0*(Ly*1000 -y);            % Add in zonal mean part of psi
    [dxzeta,dyzeta] = grad(zeta,dx,dy);   % gradient zeta for advection terms
    [dxpsi,dypsi] = grad(psi,dx,dy);      % computes gradient of psi
    v=dxpsi;                              % v = +d(psi)/dx
    if rem(tt,1) == 0;
        
        str = num2str(t/3600);            % show output  time
        str1 = ['time in hours =' str];
        omax = 1.e-7*max(max(abs(psi)))*.9;
        omin = 1.e-7*min(min(abs(psi)))*1.1;
        V = [omin:omax/6:omax];
        omax1 = 1.1e5*max(max(abs(zeta))) ;
        V2 = [ omax1/8:omax1/8:omax1];
        V3 = [-omax1:omax1/8:-omax1/8];
        
        
        figure(1)
        contour(x/1000,y/1000,psi*1.e-7,V,'b') % x, y, converted to km for graph
        hold on
        contour(x/1000,y/1000,htop,[h0/2 h0/2],'r')
        hold on
        contour(x/1000,y/1000,zeta*1.e5,V2,'k')
        contour(x/1000,y/1000,zeta*1.e5,V3,'k--')
        xlabel('x  (km)'), ylabel('y  (km)')
        title('streamfunction (blue), vorticity (black: solid positive, dashed negative)')
        text(0,-1.e4,str1)
        hold off
    end
end



