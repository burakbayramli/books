function mit18086_fd_transport_growth(method)
%MIT18086_FD_TRANSPORT_GROWTH
%    Applies various finite difference methods for the transport
%    equation u_t=u_x. Additionally shows the von Neumann growth
%    factor in complex plane.
%    Choose method by giving number as parameter. Methods of
%    choice are (1) upwind, (2) downwind, (3) centered,
%    (4) Lax-Friedrichs, (5) Lax-Wendroff, and (6) Crank-Nicolson.

% 03/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

n = 90;                            % number of space gridpoints
dt = 1e-2;                                          % time step
tf = 2.4;                                          % final time
if nargin<1, method = 1; end
x = linspace(-1,1,n)';
h = x(2)-x(1);
r = dt/h; disp(sprintf('Courant number: %0.2f',r))
u = ic(x); u0 = u;
I = eye(n);
R = diag(ones(1,n-1),1);
Dxc = (R+diag([-1 zeros(1,n-2) 1])-R')/(2*h);
Dxx = (R-diag([1 2*ones(1,n-2) 1])+R')/h^2;
switch method
case 1
   name = 'upwind';
   A = (R-I)/h;
case 2
   name = 'downwind';
   A = (I-R')/h;
case 3
   name = 'centered';
   A = Dxc;
case 4
   name = 'Lax-Friedrichs';
   A = Dxc+(h^2/dt/2)*Dxx;
case 5
   name = 'Lax-Wendroff';
   A = Dxc+(dt/2)*Dxx;
case 6
   name = 'Crank-Nicolson';
   M = (I-(dt/2)*Dxc)\(I+(dt/2)*Dxc);
end
if not(exist('M')), M = I+dt*A; end
t = linspace(0,2*pi,100);
nh = floor(size(M,1)/2)-1;
g = M(nh+1,1:2*nh+1)*exp(i*(-nh:nh)'*t);
for tn = 1:ceil(tf/dt)
   u = M*u;
   clf
   subplot(1,3,1:2)
   plot(x,u0,'b:',x,u,'r.-')
   axis([-1 1 -.1 1.1])
   title(sprintf('%s , t=%0.2f',name,tn*dt))
   subplot(1,3,3)
   patch(cos(t),sin(t),.9*[1 1 1])
   hold on, plot(g,'r-','linewidth',2), hold off
   axis equal
   axis(1.5*[-1 1 -1 1])
   title('growth factor')
   drawnow
end

function y = ic(x)
% initial condition function
y = max((1-4*(x-.5).^2),0).^2;
