function sol = ch3ex2
solinit = bvpinit(linspace(-pi/2,pi/2,20),0.5,1);
sol = bvp4c(@ode,@bcs,solinit);
fprintf('lambda = %g.\n',sol.parameters)
%plot(sol.x,sol.y(1,:));
plot(sol.x,sol.y(1,:),'-k');
axis([-pi/2 pi/2 0 1.1]);
%print -depsc ch3fig2
%=================================================
function dydx = ode(x,y,lambda)
epsilon = 0.1;
dydx = (sin(x)^2 - lambda*sin(x)^4/y)/epsilon;

function res = bcs(ya,yb,lambda)
res = [ ya-1; yb-1 ];