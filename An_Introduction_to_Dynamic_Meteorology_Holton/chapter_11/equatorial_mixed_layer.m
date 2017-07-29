% MATLAB file:  equatorial_mixed_layer.m   for use with Chapter 11
% revised 12/18/02
% Finite difference model for linear equatorial shallow water model.
% Given a geopotential field corresponding to the n = 0 Rossby-gravity
% mode, leapfrog scheme is used to compute the mixed layer response.
% Domain is equatorial beta-plane channel periodic in x.
clear all
close all
Lx = 10000;  Ly = 4000;             % channel dimensions in km
Nx = 41; Ny = 41 ;                  % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx = linspace(-Lx/2,Lx/2,Nx);       % Nx gridpoints in x   
yy = linspace( -Ly/2,Ly/2,Ny);      % Ny gridpoints in y
%convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system    
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy = Ly/(Ny-1)*1000;                % grid distance in y
%define constants and parameters
rad = 6.37e6;                   % radius of earth
omega = 7.292e-5;               % earth angular velocity
beta = 2*omega/rad;             % beta effect
A = 5;                          % forcing amplitude
alpha = 5.e-6;                  % damping rate
k = 2*pi/(Lx*1000);             % zonal wavenumber
cB = 18;                        % shallow water wave speed =sqrt(gh)
%set initial boundary layer fields to zero
u0 = zeros(size(x)); u = u0; un = u0;
v0 = zeros(size(x)); v = v0; vn = v0;
div = zeros(size(x));
dphix = u0; dphiy = u0;
freq = k*cB*(0.5-0.5*sqrt(1+4*beta/(k^2*cB)))
%%  time integration parameters:
nsv = 0 ;                       % counter for output figures
t = 0;
time_end = input('specify ending time  in hours (at least 120) time_end =   ');
time_endsec = time_end*3600;   ;    % ending time in seconds
dt = 1800;                          % time increment in seconds
dt2 = 2*dt;  
adt = 1-alpha*dt;
adt2 = 1-alpha*dt2;
tsave = 3600*6; % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);        % total number of time steps
nsave = fix(time_endsec/tsave);     % number of times saved to output

%take forward step as first step
% Begin time looping using leapfrog method
for s=1:ntime
    t = t + dt;
    phi=real(freq*y*A.*exp(i*(k*x-freq*t))).*exp(-beta/(2*cB).*y.^2);
    [dphix, dphiy] = grad(phi,dx,dy); % gradient of geopotential
    
    tt = t/tsave;
    if rem(tt,1) == 0;               % forward step to damp computational mode
        un = adt*u +dt*(beta*y.*v-dphix);
        vn = adt*v+dt*(-beta*y.*u-dphiy);
        [dux, duy] = grad(u,dx,dy);
        [dvx, dvy] = grad(v,dx,dy);
        div = dux+dvy;
    else                             %leapfrog step
        un = adt2*u0 +dt2*(beta*y.*v-dphix);
        vn = adt2*v0+dt2*(-beta*y.*u-dphiy);
        [dux, duy] = grad(u,dx,dy);
        [dvx, dvy] = grad(v,dx,dy);
        div = dux+dvy;
    end
    % update all fields
    u0 = u;  v0 = v;  
    u = un;  v = vn; 
    if rem(tt,1) == 0;
        
        time = tt*tsave/3600;          % show output  every 3 hours
        figure(1)
        subplot(2,1,1)
        pcolor(x/1000,y/1000,phi/9.8)  % x, y, converted to km for graph 
        shading interp
        xlabel('x  (km)'), ylabel('y  (km)')
        title( 'perturbaton height (red positive, blue negative')
        subplot(2,1,2)
        % Reduces number of grid points for plotting velocity vectors
        for iy = 1:Nyl/2
            for j = 1:Nxl/2
                xd(iy,j) = x(2*iy,2*j);
                yd(iy,j) = y(2*iy,2*j);
                uxd(iy,j) = u(2*iy,2*j);
                vyd(iy,j) = v(2*iy,2*j);
            end
        end   
        quiver(xd/1000,yd/1000,uxd,vyd)
        axis([-Lx/2 Lx/2 -Ly/2  Ly/2  ])
        title('velocity field and divergence')
        xlabel('x  (km)'), ylabel('y  (km)')
        hold on
        contour(x/1000,y/1000,div)
        hold off
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



