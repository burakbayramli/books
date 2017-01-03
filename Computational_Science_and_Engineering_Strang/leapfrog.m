%2.2  leapfrog.m

%Here is a short leapfrog code to solve the scalar equation u'' = -9u. 
%The exact solution u = sin 3t comes back to u = 0 at time 2pi. 
%Test n = 9,10 for stability and n = 40:40:160 for accuracy C/n^2.

function u=leapfrog(n)
dt=2*pi/n;                % n time steps to t = 2pi    
uold=0;u=3*dt;            % Starting values u(0)=0 and u(dt)=3*dt
for i=2:n 
  unew=2*u-uold-9*dt^2*u; % Leapfrog formula with u_n+1 - 2u_n + u_n-1 
  uold=u;u=unew;          % Update u's for the next time step
end
u                         % u_n approximates u(2pi)=sin(6pi)=0 
C=n^2*u                   % C is steady for second order accuracy 

