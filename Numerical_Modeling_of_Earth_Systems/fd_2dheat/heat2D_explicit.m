% Solves the 2D heat equation with an explicit finite difference scheme
clear
%Physical parameters
L       =   150e3;      %   Width of lithosphere    [m]     
H       =   100e3;      %   Height of lithosphere   [m] 
Tbot    =   1300;       %   Temperature of bottom lithosphere  [C]
Tsurf   =   0;          %   Temperature of country rock        [C]
Tplume  =   1500;       %   Temperature of plume               [C] 
kappa   =   1e-6;       %   Thermal diffusivity of rock        [m2/s]
Wplume  =   25e3;       %   Width of plume                     [m]
day     =   3600*24;    %   # seconds per day
year    =   365.25*day; %   # seconds per year

% Numerical parameters
nx      =   101;         %   # gridpoints in x-direction
nz      =   51;         %   # gridpoints in z-direction
nt      =   500;          %   Number of timesteps to compute
dx      =   L/(nx-1);   %   Spacing of grid in x-direction
dz      =   H/(nz-1);   %   Spacing of grid in z-direction
[x2d,z2d] = meshgrid(-L/2:dx:L/2, -H:dz:0);  % create grid
% Compute stable timestep
dt         = min([dx,dz])^2/kappa/4; 
% Setup initial linear temperature profile
T       =   abs(z2d./H)*Tbot;  
% Imping plume beneath lithosphere
ind      =  find(abs(x2d(1,:)) <= Wplume/2);
T(1,ind) =  Tplume;
time    =   0;
for n=1:nt

    % Compute new temperature 
    Tnew = zeros(nz,nx);
    sx  = kappa*dt/dx^2;
    sz  = kappa*dt/dz^2;
    for j=2:nx-1
        for i=2:nz-1
            Tnew(i,j) = ????;
        end
    end
    % Set boundary conditions
    Tnew(1,:)   =  T(1 ,: );
    Tnew(nz,:)  =  ?;
    for i=2:nz-1
        Tnew(i,1) = ?
        Tnew(i,nx) = ?
    end
    T           =   Tnew;
    time        =   time+dt;
    % Plot solution every 50 timesteps
    if (mod(n,50)==0)
        figure(1), clf
        pcolor(x2d/1e3,z2d/1e3,Tnew); shading interp, colorbar
        hold on
        contour(x2d/1e3,z2d/1e3,Tnew,[100:100:1500],'k');
        xlabel('x [km]')
        ylabel('z [km]')
        zlabel('Temperature [^oC]')
        title(['Temperature evolution after ',num2str(time/year/1e6),' Myrs'])
        drawnow
    end
end



