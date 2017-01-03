function mit18086_fd_transport_limiter(method)
%MIT18086_FD_TRANSPORT_LIMITER
%    Solves the transport equation u_t+cu_x=0 by finite
%    difference methods. Of interest are discontinuous initial
%    conditions. Implemented methods are (1) upwind,
%    (2) Lax-Friedrichs, (3) Lax-Wendroff, (4) Lax-Wendroff
%    with van Leer flux limiter, and (5) Lax-Wendroff with
%    Superbee flux limiter.

% 03/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

n = 160;                           % number of space gridpoints
dt = 4e-3;                                          % time step
tf = 1;                                            % final time
c = 1.1;                                   % speed of transport
if nargin<1, method = 1; end
warning('off','MATLAB:divideByZero');
x = linspace(-1,1,n)';
h = x(2)-x(1);
u = ic(x); u0 = u;

for tn = 1:ceil(tf/dt)
   theta = diffl(u)./(diffr(u)+3e-14);
   switch method
   case 1
      name = 'upwind';
      f = c*u;
   case 2
      name = 'Lax-Friedrichs';
      f = c*sumr(u)/2-h/2/dt*diffr(u);
   case 3
      name = 'Lax-Wendroff';
      f = c*sumr(u)/2-c^2*dt/2/h*diffr(u);
   case 4
      name = 'Lax-Wendroff von Leer';
      phi = limiter(theta,1);
      f = c*u+c/2*(1-c*dt/h)*diffr(u).*phi;
   case 5
      name = 'Lax-Wendroff Superbee';
      phi = limiter(theta,2);
      f = c*u+c/2*(1-c*dt/h)*diffr(u).*phi;
   end
   u = u-dt*diffl(f)/h;
   clf
   plot(x,u0,'b:',x,ic(x-c*tn*dt),'b-',x,u,'r.-')
   axis([-1 1 -.2 1.2])
   title(sprintf('%s , t=%0.2f',name,tn*dt))
   drawnow
end

function y = diffl(x)
y = [0;diff(x)];
function y = diffr(x)
y = [diff(x);0];
function y = sumr(x)
y = [x(1:end-1)+x(2:end);2*x(end)];

function y = ic(x)
% initial condition function
y = (x>-.7&x<-.2)*.8+(x>-.9&x<-.6)*.2;

function phi = limiter(r,method)
% flux limiter
if nargin<1, method = 1; end
switch method
case 1
phi = (abs(r)+r)./(1+abs(r));                        % van Leer
case 2
phi = max(0,max(min(1,2*r),min(r,2)));               % Superbee
end
