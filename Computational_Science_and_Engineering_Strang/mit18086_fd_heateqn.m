function mit18086_fd_heateqn
%MIT18086_FD_HEATEQN
%    Solves the heat equation u_t=u_xx by finite differences.
%    Example uses explicit Euler in time, homogeneous
%    Dirichlet b.c. on the left, and homogeneous Neumann b.c.
%    on the right.

% 02/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

n = 13;         % number of space gridpoints without boundaries
dt = 1e-2;                                          % time step
tf = 2;                                            % final time
x = linspace(-1,1,n+2)';
xi = x(2:end-1);
h = x(2)-x(1);
disp(sprintf('CFL number: %0.2f',dt/h^2))
u0 = f(x);
u = u0(2:end-1);
I = eye(n);
D = 2*I;
D(end,end) = 1;      % Neumann boundary condition on right side
R = diag(ones(1,n-1),1);
A = (R-D+R')/h^2;
M = I+dt*A;  % change to M = inv(I-dt*A) for implicit time step
for tn = 1:ceil(tf/dt)
   u = M*u;
   clf
   plot(x,u0,'b:',xi,u,'r.-')
   title(sprintf('time t=%0.2f',tn*dt))
   drawnow
end

function y = f(x)
% initial condition function
y = (1-x.^2).^2;
