function data = PDE1DBCData(loadCase, time)
% for elliptic, and parabolic PDEs essential BC is on u (primary field)
% for hyperbolic PDE, essential BC is on u' (time derivative, e.g. vel from
% disp)
if nargin < 1
    loadCase = 0;
end
if nargin < 2
    time = 0;
end

if (loadCase == 0)
    data = [0, 1];
elseif (loadCase == 1)
    data = [0, 0];
elseif (loadCase == 2) % source term sin(pi/2 x), T(0) = 0, q(1) = 0 
    data = [0, 0];
elseif (loadCase == 3) % exact solution IC: sin(x) hyperbolic and parabolic with periodic BC - domain x = 0 - 2pi
    data = [0, 0];
elseif (loadCase == 4) % 2 material example, stress load on the left edge
    data = [-1, 0];
end
