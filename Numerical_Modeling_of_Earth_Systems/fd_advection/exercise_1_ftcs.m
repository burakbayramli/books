%
% FTCS advection schem
%
clear all

nx      =   201;
W       =   40;     % width of domain
Vel     =   -4;      % velocity
sigma   =   1;
Ampl    =   2;
nt      =   500;    % number of timesteps
dt      =   1e-2;   % timestep
dx      =   W/(nx-1);
x       =   0:dx:W;
% Initial Gaussian T-profile
xc      =   20;
T       =   Ampl*exp(-(x-xc).^2/sigma^2);
% Velocity
Vx      =   ones(1,nx)*Vel;
abs(Vel)*dt/dx
cfac = dt/(2*dx);
% Central finite difference discretization
for itime=1:nt    
    % central fin. diff
    for ix=2:nx-1
      Tnew(ix) = T(ix) - Vx(ix)*cfac *(T(ix+1)-T(ix-1));
    end
    % BCs
    Tnew(1)     =   T(1);
    Tnew(nx)    =   T(nx);
    T           =   Tnew;
    time        =   itime*dt;
    % Analytical solution for this case
    T_anal      =   Ampl*exp(-(x-xc-time*Vel).^2/sigma^2);
    figure(1),clf, plot(x,T,x,T_anal), ...
	legend('Numerical','Analytical')
    xlabel('x')
    ylabel('temperature')
    drawnow
end

