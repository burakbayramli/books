% control_1D_HJB.m
% This program solves the HJB equation for
% a simpe 1-D optimal control problem using
% upwind finite differencing.
%
% INPUT arguments
% ---------------
% SYS: definition of system and tuning parameters
%   SYS.tH: horizon time
%   SYS.CU: penalty weight for control inputs
%   SYS.CH: horizon-time penalty weight
%   SYS.xSet: set-point of system
%   SYS.uSet: control input that achieves set-point steady-state
%   SYS.numReport: number of times at which Bellman function is
%                  to be reported
% GRID: definition of numerical grid for HJB FD calculation
%   GRID.N: number of point in the grid
%   GRID.xLo: lower-bound on computational domain
%   GRID.xHi: upper-bound on computational domain
%
% OUTPUT arguments
% ----------------
% UC: optimal control law
%   UC.x: x-values
%   UC.u: u-values for optimal control law
%   UC.K: effective proportional controller gain
%
% Kenneth J. Beers
% MIT Department of Chemical Engineering
% June 6, 2004

function [UC,TRAJ,iflag] = control_1D_HJB(SYS,GRID);
iflag = 0;

% set default values if necessary
% system parameters
if(exist('SYS') ~= 1)
    SYS = [];
end
if(~isfield(SYS,'tH'))
    SYS.tH = 10;
end
if(~isfield(SYS,'CU'))
    SYS.CU = 1;
end
if(~isfield(SYS,'CH'))
    SYS.CH = 10;
end
if(~isfield(SYS,'xSet'))
    SYS.xSet = 2;
end
if(~isfield(SYS,'uSet'))
    SYS.uSet = 1;
end
if(~isfield(SYS,'numReport'));
    SYS.numReport = 50;
end
% grid parameters
if(exist('GRID') ~= 1)
    GRID = [];
end
if(~isfield(GRID,'N'))
    GRID.N = 100;
end
if(~isfield(GRID,'xLo'))
    GRID.xLo = -10;
end
if(~isfield(GRID,'xHi'))
    GRID.xHi = 10;
end
% initialize numerical grid values
GRID.x = linspace(GRID.xLo,GRID.xHi,GRID.N)';
GRID.dx = GRID.x(2)-GRID.x(1);
% compute finite difference matrix based on forward difference
% for x<=0 and backward difference for x>0
GRID.A = spalloc(GRID.N,GRID.N,2);
for k=1:GRID.N
    if(GRID.x(k) <= 0)  % forward difference
        GRID.A(k,k+1) = 1;  GRID.A(k,k) = -1;
    else  % backward difference
        GRID.A(k,k) = 1;  GRID.A(k,k-1) = -1;
    end
end
GRID.A = GRID.A./GRID.dx;


% get initial values of Bellman function at tau = 0
phi0 = SYS.CH.*(GRID.x - SYS.xSet).^2;

% set tau values at which the Bellman function is to
% be reported
tau_report = linspace(0,SYS.tH,SYS.numReport);
% call ode23s to solve HJB equation
[TRAJ.tau,TRAJ.phi] = ode23s(@phi_dot,tau_report,phi0,[],SYS,GRID);

% plot results of Bellman function trajectory
[XX,TT] = meshgrid(GRID.x,TRAJ.tau);
figure; surf(XX,TT,TRAJ.phi);
xlabel('x');  ylabel('tau');
title('Bellman function \phi(\tau,x)');

% reconstruct optimal control law
UC.x = GRID.x;
[phi_dot,UC.u] = phi_dot(SYS.tH,TRAJ.phi(SYS.numReport,:)',SYS,GRID);
figure; plot(UC.x,UC.u);
hold on;
% add a line at u = SYS.uSet
plot([GRID.xLo GRID.xHi],[SYS.uSet SYS.uSet], 'k:');
% add a line at x = SYS.xSet
plot([SYS.xSet SYS.xSet], [min(UC.u) max(UC.u)], 'k-.');
xlabel('x');  ylabel('u');  axis tight;
title('Optimal control law');

% Fit the resulting control law to a simple proportional controller
e = UC.x - SYS.xSet;  % departure from set point
UC.K = dot(e,UC.u-SYS.uSet)/dot(e,e);

iflag = 1;
return;


% ------------------------------
% This routine returns the tau derivative of the Bellman function.
% An optimal second output is the optimal control values
function [phi_dot,u] = phi_dot(tau,phi,SYS,GRID);

% compute first-pass of derivatives dphi_dx from finite differences
dphi_dx = GRID.A*phi;
% compute optimal control inputs
u = SYS.uSet - dphi_dx./SYS.CU;
% compute time derivative vector
f = -(GRID.x-1) + u;
% recompute derivatives to ensure upwind differencing
for k=2:(GRID.N-1)
    % if f >0, use forward difference
    if(f(k) > 0)
        dphi_dx(k) = (phi(k+1)-phi(k))/GRID.dx;
    else  % use backward difference
        dphi_dx(k) = (phi(k)-phi(k-1))/GRID.dx;
    end
end
% get new optimal control inputs and time derivative vector
% based on change in direction of differences
u = SYS.uSet - dphi_dx./SYS.CU;
f = -(GRID.x-1) + u;

% compute tau derivatives of Bellman function
phi_dot = (SYS.CU/2).*(u - SYS.uSet).^2 ...
    + (GRID.x - SYS.xSet).^2 ...
    + dphi_dx.*f;

return;


