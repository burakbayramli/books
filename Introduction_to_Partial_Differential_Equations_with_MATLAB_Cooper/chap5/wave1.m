

%                            Program wave1
%
%      This program uses a finite difference scheme to integrate the 
%   wave equation  u_tt + 2du_t -u_xx +ku = 0 on [0,10] with
%   u = 0 at both ends.
%      At run time, user enters the values of d and k. delx and delt
%   are both set at .025. At run time user enters the number n1 of
%   time steps to the first snapshot, snap1; n2 the number of time
%   steps from the first snapshot to the second snapshot, etc.
%   For example if n1 = 20, n2 = 20, n3 = 10 and n4 = 40, then
%   snapshots will be made at times t1 = .5, t2 = 1.0, t3 = 1.5, and
%   t4 = 2.5.
%      Two choices of initial data are built into the program; they
%   are the plucked string, and the hammer blow.  For general data
%   user must provide function files f.m and g.m which are array
%   smart and such that f(0) = f(10) = 0 , g(0) = g(10) = 0.

d = input(' enter the value of d  ')
k = input(' enter the value of k  ')

    disp ('  enter choice of data  ')
disp('1 and 2 are piecewise data, while 3 must be array smart ')
m = input(' 1 for hammer blow,  2 for plucked string, 3 for other    ')

disp(' Note: time step here is delt = .025  ')

disp('Enter the number of time steps between snapshots ')
n = input('in the form [n1 n2 n3 n4]   ')
delt = .025;
delx = .025;

n1 = n(1); n2 = n(2); n3 = n(3); n4 = n(4);

t1 = n1*delt
t2 = t1 +n2*delt
t3 = t2 + n3*delt
t4 = t3 + n4*delt

rho = (delt/delx)^2;
denom = 1.0 + d*delt +.5*k*(delt)^2;
alpha = 2*(1.0 - rho)/denom;
beta = rho/denom;
gamma = (1.0 -d*delt +.5*k*(delt)^2)/denom;

a = 1.0 -.5*k*(delt)^2 -rho;
b = (1.0 -d*delt)*delt;
 
x = 0:delx:10;
J = 10/delx;
j = [2:J];



if m == 1 

% initial conditions for hammer blow
% initial position
  snap0 = zeros(size(x));
  w = snap0;

% determination of initial velocity
  gg = zeros(size(x));
  for i = J/4+1:3*J/4 + 1
       gg(i) = -1;
   end
   gg(J/4+ 1) = -.5;
   gg(3*J/4 + 1) = -.5;

% first time step for hammer blow.
   v = b*gg;

elseif m == 2

% initial data for plucked string.
   for i = 1:J/2
       snap0(i) = x(i)/5;
   end
   for i = J/2 + 1: J+1
       snap0(i) = (10-x(i))/5;
   end
   w = snap0;

%first time step for plucked string
   v = zeros(size(x));
   v(j) = a*w(j) +.5*rho*(w(j+1) + w(j-1));

else m == 3

% This third option uses array smart data 
% initial data
    snap0 = f(x);
    w = snap0;
    v = zeros(size(x));
%first time step
    v(j) = a*f(x(j)) +.5*rho*( f(x(j-1)) +f(x(j+1)) ) +b*g(x(j));
end

u = zeros(size(x));

% here begins the loop for later time steps
for n =  1: n1
   u(j) = alpha*v(j) + beta*(v(j+1) + v(j-1) ) - gamma*w(j);
   w = v;
   v = u;
end
disp(' Computed up to time t1 ')
snap1 = u;


for n =  1: n2 
   u(j) = alpha*v(j) + beta*(v(j+1) + v(j-1) ) - gamma*w(j);
   w = v;
   v = u;
end
disp(' Computed up to time t2' )
snap2 = u;

for n = 1: n3 
    u(j) = alpha*v(j) + beta*(v(j+1) + v(j-1) ) - gamma*w(j);
    w = v;
    v = u;
end
disp(' Computed up to time t3 ')
snap3 = u;

for n = 1: n4
    u(j) = alpha*v(j) + beta*(v(j+1) + v(j-1) ) - gamma*w(j);
    w =v;
    v = u;
end
disp(' Computed up to time t4  ')
snap4 = u ;
plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)
