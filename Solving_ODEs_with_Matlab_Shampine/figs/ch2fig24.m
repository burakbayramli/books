function ch2ex3
% The ball is at (x(t),y(t)). 
% Here y(1) = x(t), y(2) = x'(t), 
%      y(3) = y(t), y(4) = y'(t).
% Set initial height and coefficient of restitution:
y0 = [0; 0; 2; 0];
k = 0.35;
% Plot the initial configuration:
fill([0 0 1],[0 1 0],[0.8 0.8 0.8]);
axis([-0.1 1.1 0 y0(3)])
hold on
%plot(0,y0(3),'ro');  % Mark the initial point.
plot(0,y0(3),'ko');  % Mark the initial point.
options = odeset('Events',@events);
% Accumulate the path of the ball in xplot,yplot.
xplot = [];
yplot = [];
tstar = 0;
while 1
	tspan = linspace(tstar,tstar+1);
	[t,y,te,ye,ie] = ode23(@odes,tspan,y0,options);
    % Accumulate the path.
    xplot = [xplot; y(:,1)];
    yplot = [yplot; y(:,3)];
	if isempty(ie)          % Extend the interval.
		tstar = t(end);
		y0 = y(end,:);
	elseif ie(end) == 1     % Ball bounced.
%       plot(ye(end,1),ye(end,3),'r*'); % Mark the bounce point.
		plot(ye(end,1),ye(end,3),'k*'); % Mark the bounce point.
		if (te(end) - tstar) < 0.01*tstar
			fprintf('Bounces accumulated at x = %g.\n',ye(end,1))
			break;
		end
		if abs(ye(end,1) - 1) < 0.01
			break;
		end
		tstar = te(end);
    	y0 = [ye(end,1); -k*ye(end,4); ye(end,3); k*ye(end,2)];
	elseif ie(end) == 2     % Reached end of ramp.
		break;	
	end
end
% Plot the solution.
%plot(xplot,yplot);
plot(xplot,yplot,'-k');
%print -depsc ch2fig3

%==================================================
function dydt = odes(t,y)
% g = 9.81.
dydt = [y(2); 0; y(4); - 9.81];

function [value,isterminal,direction] = events(t,y)
value = [ y(3) - (1 - y(1))
              y(1) - 1     ];
isterminal = [1; 1];
direction  = [-1; 0];