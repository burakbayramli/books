function mit18086_fd_waveeqn
%MIT18086_FD_WAVEEQN
%    Solves the wave equation u_tt=u_xx by the Leapfrog method.
%    Example uses homogeneous Dirichlet b.c. on the left (fixed
%    end) and homogeneous Neumann b.c. right (loose end).

% 02/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

n = 101;         % number of space gridpoints without boundaries
dt = 1.6e-2;                                         % time step
tf = 8e0;                                           % final time
x = linspace(-1,1,n+2)'; x = x(2:end-1);
h = x(2)-x(1);
r = dt/h;
disp(sprintf('CFL number: %0.2f',r))

u0 = f(x);
U = [u0,u0+dt*g(x)];

I = eye(n);
R = diag(ones(1,n-1),1);
A = r^2*R+2*(1-r^2)*I+r^2*R';
A(end,end) = 2-r^2;
for tn = 3:ceil(tf/dt)
   U = [U(:,2),A*U(:,2)-U(:,1)];
   clf
   plot(x,u0,'b:',x,U(:,2),'r.-')
   axis([-1 1 -1.2 1.2])
   title(sprintf('time t=%0.2f',tn*dt))
   drawnow
end

function y = f(x)
% initial displacement
y = max(1-20*x.^2,0).^2;

function y = g(x)
% initial velocity
y = -[diff(f(x));0]/(x(2)-x(1))/2;
