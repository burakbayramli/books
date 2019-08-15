function void = ch1ex1(void)
% First example of Chapter 1.  
% Motion of a pendulum treated as an IVP.
set(gcf,'DefaultAxesColorOrder',[0 0 0; 0 0 0; 0 0 0])
plot([0,7],[0,0])
axis([0 7 -3 8])
hold on
thetap0 = [1.5, 1.9, -1.9 2.1, 2.5];
for j = 1:length(thetap0)
	[t,theta] = ode45(@pendulum,[0 10],[0; thetap0(j)]);
	%plot(t,theta(:,1))
    plot(t,theta(:,1),'-k','LineWidth',1)
end
% Find where the solution with thetap0 = 1.9 vanishes.
% Solutions are odd functions, so it crosses the solution
% with thetap0 = -1.9 there.  The equation is homogeneous,
% so both cross the solution with thetap0 = 0 there. This 
% gives three solutions to the BVP with zero values as BCs 
% at t = 0 and the point found in this way.
opts = odeset('Events',@pendulumev);
[t,theta,te] = ode45(@pendulum,[0 10],[0; 1.9],opts);
%plot(te(1),0,'ro',);
plot(te(1),0,'ko','LineWidth',1);
fprintf('The solutions with initial slopes 1.9, -1.9 cross at t = %e.\n',te(1));
% Now compute the equilibrium solution as a BVP.
infinity = 100;
solinit = bvpinit(linspace(0,infinity,10),[3 0]);
sol = bvp4c(@pendulum,@pendulumbc,solinit);
%plot(sol.x,sol.y(1,:),'r')
plot(sol.x,sol.y(1,:),':ko','MarkerSize',2,'MarkerFaceColor','k')
hold off
%print -depsc ch1fig1
fprintf('Initial slope of equilibrium solution is computed to be %e.\n',sol.y(2,1));
%=============================================
function yp = pendulum(t,y)
yp = [y(2); -sin(y(1))];

function res = pendulumbc(ya,yb)
res = [    ya(1)
	    yb(1) - pi ]; 

function [value,isterminal,direction] = pendulumev(t,y)
value = y(1);
isterminal = 0;
direction = -1;