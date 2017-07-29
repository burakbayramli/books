% MATLAB file:  barotropic_model_4.m   for use with Chapter 13.
% Finite difference model for barotropic vorticity equation
% uses localized vorticity as initial condition and leapfrog differencing.
% Given a vorticity pattern, computes the streamfunction by
% inversion of the Laplacian using Fourier transform in x and FD in y
% Routine calls function "stream1.m" to invert vorticity and
% then plots vorticity and streamfunction.
% Note that domain is beta-plane channel periodic in x.
% Velocity zero at northern and southern boundaries.
clear all
close all
Lx = 6000;  Ly = 3000;              % channel dimensions in km
Nx = 65; Ny = 33 ;                  % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx = linspace(-Lx/2,Lx/2,Nx);       % Nx gridpoints in x   
yy = linspace( -Ly/2,Ly/2,Ny);      % Ny gridpoints in y
%convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
k = 2*pi/(Lx*1000) ;                % zonal wavenumber in units of 1/ m
m = pi/(Ly*1000);                   % meridional wavenumber
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy = Ly/(Ny-1)*1000;                % grid distance in y
U0 = 20;                            % mean zonal wind
beta = 0* 1.62e-11;                 % beta effect
Av4 = 1.e-6;                        % 4th order diffusion
A = 1.e-4;                          % wave vorticity amplitude
% compute initial vorticity and streamfunction
zeta0 = A*exp(-2*(k^2*x.^2+m^2*y.^2));
zeta = zeta0; zetan = zeta0;
%%  time integration parameters:
nsv = 0;                            % counter for output figures
t=0;
time_end = input('specify ending time of integration in hours time_end =   ');
time_endsec = (time_end+1)*3600;    % ending time in seconds
dt = 900;                           % time increment in seconds

tsave = 3600*3;     % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);        % total number of time steps
nsave = fix(time_endsec/tsave);     % number of times saved to output

% call stream1.m to compute stream function from vorticity
psi = stream1(Nxl,Nyl,Lx,dy,zeta);
psi = psi+U0*(Ly/2*1000 -y);
[dxpsi,dypsi] = grad(psi,dx,dy);        % computes gradient of psi
u = -dypsi;                             % u = -d(psi)/dy
v = dxpsi;                              % v = +d(psi)/dx
[dflx,dfly] = divflux(zeta,u,v,dx,dy);  % flux divergence
omax1 = 1.e-7*max(max(psi))*.9;
V = [0:omax1/6:omax1];
omax = 1.1e5*max(max(zeta));
V2 = [2 4 6 8];
V3 = [-2 -4 -6 -8];
% Begin time looping using leapfrog method
for s = 1:ntime
    tt = t/tsave;
    t = t + dt;   
    numdif = diff4xy(Av4,Nx,Ny,zeta0);  % 4th order damping  
    if rem(tt,1) == 0   % forward step to damp computational mode
        
        zetan(2:Nyl,1:Nxl) = zeta(2:Nyl,1:Nxl)-beta*dt*v(2:Nyl,1:Nxl) ...
            - dt*(dflx(2:Nyl,1:Nxl)+dfly(2:Nyl,1:Nxl))...
            -dt*numdif(2:Nyl,1:Nxl);
    else 
        zetan(2:Nyl,1:Nxl) = zeta0(2:Nyl,1:Nxl) -beta*2*dt*v(2:Nyl,1:Nxl)...
            -2*dt*(dflx(2:Nyl,1:Nxl)+dfly(2:Nyl,1:Nxl))...
            -2*dt*numdif(2:Nyl,1:Nxl);
    end
    zetan(:,Nx)=zetan(:,1);
    zeta0 = zeta;                       % update old time
    zeta = zetan;
    % call to function to compute  stream function at new time from vorticity
    psi = stream1(Nxl,Nyl,Lx,dy,zeta);
    psi = psi+U0*(Ly/2*1000 -y);            % Add in zonal mean part of psi
    [dxpsi,dypsi] = grad(psi,dx,dy);        % computes gradient of psi
    u = -dypsi;                             % u = -d(psi)/dy
    v = dxpsi;                              % v = +d(psi)/dx
    [dflx,dfly] = divflux(zeta,u,v,dx,dy);  % flux divergence
    if rem(tt,1) == 0
        
        time = tt*tsave/3600;               % show output every 3 hours
        KE=sum(sum(u(:,1:Nxl).^2,2) +sum(v(:,1:Nxl).^2,2));
        zetb = sum(sum(zeta(:,1:Nxl),2)); 
        enst = sum(sum(zeta(:,1:Nxl).^2,2));
        format short e
        [time KE zetb enst]
        str1 = ['time = ' num2str(time) ' hours'];
        figure(1)
        contour(x/1000,y/1000,psi*1.e-7,V,'b') % x, y, converted to km for graph
        hold on
        contour(x/1000,y/1000,zeta*1.e5,V2,'k')
        contour(x/1000,y/1000,zeta*1.e5,V3,'k--')
        xlabel('x (km)'), ylabel('y (km)')
        title('streamfunction (blue),   vorticity (black)')
        text(0, 1200, str1)
        hold off
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



