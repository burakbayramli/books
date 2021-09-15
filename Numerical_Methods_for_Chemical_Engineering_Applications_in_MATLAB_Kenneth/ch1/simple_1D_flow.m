% simple_1D_flow.m
% This MATLAB program employs the finite difference method
% to compute the velocity profile of a Newtonian fluid in
% pressure-driven flow between two infinite flat plates,
% the upper of which is moving at a specified velocity.
% K. Beers. MIT ChE. 9/4/03

function iflag_main = simple_1D_flow();
iflag_main = 0;

% set the system parameters
visc = 1e-3;  % viscosity in Pa*s
rho = 1000;  % density in Kg/m^3
V_up = 0;  % vel. of upper plate in m/s
B = 1/1e3;  % distance between plates in m
dp_dx = input('Enter dp/dx in Pa/m : ');

% set simulation parameters
N = 25;
dy = B/(N+1);

% set matrix
A = spalloc(N,N,3*N);
v = ones(N,1);
A = spdiags([-v 2*v  -v], -1:1, N, N);

% set RHS vector
G = -(dy^2)/visc*dp_dx;
b = G*ones(N,1);
b(N) = b(N) + V_up;
    
% Solve system
v = A\b;

% compute Reynolds number
v_avg = mean(abs(v));
Re = rho*v_avg*(2*B)/visc; disp(dp_dx); disp(Re);

% plot velocity profile
figure;
y_plot = linspace(0,B,N+2);
plot(y_plot,[0;v;V_up]);
phrase1 = ['dp/dx = ', num2str(dp_dx)];
phrase1 = [phrase1, ', Re = ', num2str(Re)];
gtext(phrase1);
if(Re > 1)
    gtext('Re > 1, flow may not be laminar');
end
xlabel('y (m)'); ylabel('v_x(y) (m/s)');
title('Laminar pressure driven flow between parallel plates');

iflag_main = 1;
return;