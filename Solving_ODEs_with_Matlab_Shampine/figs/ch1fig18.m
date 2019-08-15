function ch1ex2
% This is HB1ODE with its output modified.
%HB1ODE Stiff problem 1 of Hindmarsh and Byrne.
%   HB1ODE runs a demo of the solution of the original Robertson chemical
%   reaction problem on a very long interval. Because the components tend to
%   a constant limit, it tests reuse of Jacobians. The equations themselves
%   can be unstable for negative solution components, which is admitted by
%   the error control. Many codes can, therefore, go unstable on a long time
%   interval because a solution component goes to zero and a negative
%   approximation is entirely possible. The default interval is the 
%   longest for which the Hindmarsh and Byrne code EPISODE is stable. The
%   system satisfies a conservation law which can be monitored:  
%   
%       y(1) + y(2) + y(3) = 1
%   
%   A. C. Hindmarsh and G. D. Byrne, Applications of EPISODE: An
%   Experimental Package for the Integration of Ordinary Differential
%   Equations, in Numerical Methods for Differential Systems, L. Lapidus and
%   W. E. Schiesser eds., Academic Press, Orlando, FL, 1976, pp 147-166.
%   
%   See also ODE15S, ODE23S, ODE23T, ODE23TB, ODESET, @.

%   Mark W. Reichelt and Lawrence F. Shampine, 2-11-94, 4-18-94
%   Copyright 1984-2000 The MathWorks, Inc. 
%   $Revision: 1.13 $  $Date: 2000/06/01 03:16:28 $
tspan = [0; 0.04e9];
y0 = [1; 0; 0];
[t,y] = ode15s(@f,[0 4*logspace(-6,6)],y0);
y(:,2) = 1e4*y(:,2);
%semilogx(t,y(:,1),t,y(:,2),t,y(:,3))
semilogx(t,y(:,1),'-k',t,y(:,2),':ko',t,y(:,3),'-.k','MarkerSize',2,...
         'MarkerFaceColor','k')
legend('y_1','y_2 \times 10^4','y_3',0)
axis([0 04e6 -0.1 1.1])
% Drag the location of the legend to a better
% place and print from the command line.
%print -depsc ch1fig2
% --------------------------------------------------------------------------
function dydt = f(t,y)
dydt = [ (-0.04*y(1) + 1e4*y(2)*y(3))
         (0.04*y(1) - 1e4*y(2)*y(3) - 3e7*y(2)^2)
         3e7*y(2)^2 ];