

%                            program heat2
%
%     This program computes the solution of the heat equation 
%   u_t - ku_xx = 0 on the half line x > 0  with either Dirichlet
%   condition at x = 0 ( u(0,t) = 0) or Neumann condition at x = 0
%   ( u_x(0,t) = 0 ). The initial data is a step function 
%   f(x) = 1 for 1 < x < 2 and zero elsewhere.  The solution is 
%   written in terms of the error function.
%       At run time the use must enter the time t of the snapshot,
%   snap1. The solutions of both problems are plotted together on 
%   0 < x < 10.

disp('  ')
t = input('enter the value of t  ')
denom = sqrt(4.0*t);

x = -10:.05:10;

m = length(x);

x1 = x(1:1:220);
x2 = x(221:1:241);
x3 = x(242:1:401);

m1=  length(x1);
m2 = length(x2);
m3 = length(x3);

u1 = .5*( erf((2-x1)/denom) - erf((1-x1)/denom) );

u2 = .5*( erf((2-x2)/denom) + erf((x2-1)/denom) );

u3 = .5*( erf((x3-1)/denom) - erf((x3-2)/denom) );

u = [u1,u2,u3];

uflip = u(m:-1:1);

dirch = u - uflip;

neumann = u + uflip;

plot(x,dirch, x,neumann) 
axis([0 10 0 1])
