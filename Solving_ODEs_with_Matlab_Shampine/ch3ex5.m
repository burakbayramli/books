function ch3ex5
global c alpha beta infty
options = []; 
options = bvpset(options,'Vectorized','on');
options = bvpset(options,'FJacobian',@odeJac);
options = bvpset(options,'BCJacobian',@bcJac);
color = [ 'r', 'b', 'g', 'm', 'k' ];
wave_speed = [5, 10, 15, 20, 25];
hold on
for i = 1:5
	c = wave_speed(i);
	alpha = (-c + sqrt(c^2 + 4))/2;
    beta  = (-c + sqrt(c^2 - 4))/2;
    infty = 10*c;
%    if i == 1
        solinit = bvpinit(linspace(-infty,infty,20),@guess);
%    else
%        solinit = bvpinit(sol,[-infty,infty]);
%    end
    sol = bvp4c(@ode,@bc,solinit,options);
    plot(sol.x,sol.y(1,:),color(i));
    axis([-250 250 0 1]);
    drawnow
end
legend('c = 5', 'c = 10', 'c = 15', 'c = 20', 'c = 25',3);
hold off
%print -depsc ch3fig5
%======================================================
function v = guess(z)
global c alpha beta infty
if z > 0
    v = [exp(beta*z); beta*exp(beta*z)];
else
    v = [(1 - exp(alpha*z)); -alpha*exp(alpha*z)];
end

function dydz = ode(z,y)
global c alpha beta infty
dydz =  [ y(2,:); -(c*y(2,:) + y(1,:).*(1 - y(1,:))) ];

function dFdy = odeJac(z,y)
global c alpha beta infty
dFdy = [       0,          1
         (-1 + 2*y(1)),  - c ];

function res = bc(ya,yb)
global c alpha beta infty
res = [ ya(2)/(ya(1) - 1) - alpha
        yb(1)/exp(beta*infty) - 1 ];

% deriveFbc.m derives these partial derivatives.
function [dBCdya, dBCdyb] = bcJac(ya,yb)
global c alpha beta infty
dBCdya = [ -ya(2)/(ya(1) - 1)^2, 1/(ya(1) - 1)
                   0                   0       ];

dBCdyb = [         0          0
           1/exp(beta*infty), 0 ];