function ch1ex7
% Compute two solutions of ballistic problem.
% y(1) = y, y(2) = v, y(3) = phi.
xend = 5;
xout = linspace(0,xend);
xguess = linspace(0,xend,5);
% Constant guess that satisfies the BCs.
yguess = [0; 0.5; 0.1];
solinit = bvpinit(xguess,yguess);
opts = bvpset('RelTol',1e-4,'AbsTol',1e-7);
sol = bvp4c(@ode,@bc,solinit,opts);
plot1 = bvpval(sol,xout);
% Change the guessed initial slope to converge to a different solution. 
%yguess = [0; 0.5; 1.2];
yguess = [0; 0.5; 9.7456];
solinit = bvpinit(xguess,yguess);
sol = bvp4c(@ode,@bc,solinit,opts);
plot2 = bvpval(sol,xout);
%plot(xout,plot1(1,:),'b',xout,plot2(1,:),'r')
plot(xout,plot1(1,:),'-k',xout,plot2(1,:),':ko','MarkerSize',2)
%print -depsc ch1fig7
%===========================================================================
function dydx = ode(x,y)
% y(1) = y, y(2) = v, y(3) = phi.
g = 0.032;
nu = 0.02;
dydx = zeros(3,1);
dydx(1) = tan(y(3));
dydx(2) = - (g*sin(y(3)) + nu*y(2)^2)/(y(2)*cos(y(3)));
dydx(3) = - g/y(2)^2;

function v = bc(ya,yb)
v = [     ya(1)
	  ya(2) - 0.5
	      yb(1)    ];