

%                            Program wave2
%
%      This program uses a finite difference scheme to integrate the 
%   nonlinear wave equation  u_tt  - u_xx + gamma*u**3 = 0 on [0,10] with
%   with u = 0 at both ends. The nonlinear term is treated with a
%   scheme that preserves the discrete engergy ( see book of Strauss).
%   The resulting method is implicit, so that a system of nonlinear
%   equations must be solved at each time step.  This is done in the
%   subroutine  'newton' .  Delx and delt are set to .025.
%      At run time user enters the value of gamma ( gamma= 0) is 
%   the linear wave equation. User also enters the number 
%   n1 of time steps to the first snapshot, snap1; n2 the number of
%   time steps from the first snapshot to the second snapshot,
%   snap2, etc. Example: n1 = 20, n2 = 30, n3 = 40, n4 = 20 gives
%   snapshots at t1 = .5, t2 = 1.25, t3 = 2.25, and t4 = 2.75.
%     There are two choices of initial data: one is built in to 
%   program, the plucked string. The user must provide function
%   files f.m and g.m for the second choice.  They must be array
%   smart and must satisfy the compatibility conditions 
%   f(0) = g(0) = 0 and f(10) =  g(10) = 0.  At run time user
%   must enter the scale factor delta of the initial data so that
%   the program runs with initial data delta*f and delta*g. This 
%   also applies to the initial data for the plucked string.

gamma = input(' Enter the choice of gamma.  gamma = 0 is the linear wave eq.  ')

disp ('  enter choice of data  ')
disp('1 is piecewise data, while 2 must be array smart ')
m = input(' 1 for plucked string, 2 for other    ')

delta = input(' Enter the scale factor delta of the initial data  ')

disp(' Note: for this program the time step delt = .025  ')

disp('  Enter the number of time steps between snaphots  ')
N = input(' in the form  [n1, n2, n3, n]      ')
delt = .025;
delx = .025;

n1 = N(1); n2 = N(2); n3 = N(3); n4 = N(4);

t1 = n1*delt
t2 = t1 +n2*delt
t3 = t2+n3*delt
t4 = t3 +n4*delt

global alpha
global beta
global rho

global u
global v
global w

rho = (delt/delx)^2;
alpha = 2*(1.0 - rho);
beta = .25*gamma*(delt)^2;

 
x = 0:delx:10;
J = 10/delx;

j = [2:J];

snap0 = zeros(1,J+1);

%initial data for plucked string
if m == 1
    for i = 1:J/2
       snap0(i) = delta*x(i)/5;
    end
    for i = J/2 + 1: J+1
       snap0(i) = delta*(10-x(i))/5;
    end
w = snap0;
w3 = w.^3;
v = zeros(1, J+1);
%first time step for plucked string
v(j) = w(j) +.5*rho*( w(j+1) -2*w(j) + w(j-1) ) -.5*(delt)^2*gamma*w3(j);

elseif m == 2

% This second  option uses array smart data 
%initial data
    snap0 = delta*f(x);
    w = snap0;
    v = zeros(1,J+1);

    w3 = w.^3;
    h = delta*g(x);
%first time step
    v(j) = w(j) + delt*h(j) +.5*rho*(w(j+1) -2*w(j) +w(j-1) ) - .5*(delt)^2*gamma*w3(j);
end

% begin the loops for integration.
u= zeros(1,J+1);


for n = 2:n1
   u(j) = newton(w(j),v(j),v(j+1),v(j-1));
   w = v;
   v = u;
end
disp(' Computed up to time t1 ')
snap1 = u;


for n = 1:n2
   u(j) = newton(w(j),v(j),v(j+1),v(j-1));
   w = v;
   v = u;
end
disp(' Computed up to time t2' )
snap2 = u;

for n = 1:n3
    u(j) = newton(w(j),v(j),v(j+1), v(j-1));
    w = v;
    v = u;
end
disp(' Computed up to time t3 ')
snap3 = u;

for n = 1:n4
    u(j) = newton(w(j),v(j),v(j+1), v(j-1));
    w =v;
    v = u;
end
disp(' Computed up to time t4  ')
snap4 = u ;
plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)
