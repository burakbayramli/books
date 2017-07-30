% Solves the 2D heat equation with an implicit finite difference scheme
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
dt      =   100e6*year;   %   timestep
% Numerical parameters
nx      =   51;         %   # gridpoints in x-direction
nz      =   51;         %   # gridpoints in z-direction
nt      =   100;          %   Number of timesteps to compute
dx      =   L/(nx-1);   %   Spacing of grid in x-direction
dz      =   H/(nz-1);   %   Spacing of grid in z-direction
[x2d,z2d] = meshgrid(-L/2:dx:L/2, -H:dz:0);  % create grid
% Setup initial linear temperature profile
T       =   abs(z2d./H)*Tbot;  
% Imping plume beneath lithosphere
ind      =  find(abs(x2d(1,:)) <= Wplume/2);
T(1,ind) =  Tplume;
% Setup numbering
num      =  1;
for i=1:nz
    for j=1:nx
        Number(i,j) = num;
        num = num+1;
    end
end
% Construct the A matrix
A       = sparse(nx*nz,nx*nz);
sx      = kappa*dt/dx^2;
sz      = kappa*dt/dz^2;
for i = 2:nz-1
    for j = 2:nx-1        
        ii          = Number(i,j);
        A( ii, Number(i+1,j  ))  =   ??;
        A( ii, Number(i  ,j+1))  =   ??;
  ?? 
    end
end
% Set lower and upper BC
for j = 1:nx
??
end
% Set left and right BC 
for i = 1:nz
??
end
time    =   0;
for n=1:nt
    % Compute rhs
    rhs   = zeros(nx*nz,1);
    for i = 1:nz
        for j = 1:nx
            ii      = Number(i,j);
          ??
        end
    end
    % Compute solution vector
    Tnew_vector =   A\rhs;
    % Create 2D matrix from vector
    Tnew        =   Tnew_vector(Number);
    T           =   Tnew;
    time        =   time+dt;
    % Plot solution every 50 timesteps
    if (mod(n,10)==0)
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



