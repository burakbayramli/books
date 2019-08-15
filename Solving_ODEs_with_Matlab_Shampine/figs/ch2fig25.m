function batonode
%BATONODE  Simulate the motion of a thrown baton.
%   BATONODE solves a problem with a time- and state-dependent mass
%   matrix. This example is based on Example 4.3A of D.A. Wells, Theory and
%   Problems of Lagrangian Dynamics, McGraw-Hill, 1967.  Many problems are
%   conveniently formulated with a mass matrix.  A baton is modelled as two
%   particles of masses m1 and m2 rigidly fastened to a light rod of length
%   L.  Its motion is followed in the vertical x-y plane under the action of
%   gravity.  If the coordinates of the first particle are (X,Y) and the
%   angle the rod makes with the horizontal is theta, Lagrange's equations 
%   lead to a mass matrix that depends on the unknown theta.  The variables
%   y here are y(1)=X, y(2)=X', y(3)=Y, y(4)=Y', y(5)=theta, y(6)=theta'. 
%   
%   See also ODE45, ODE113, ODESET, @.

%   Mark W. Reichelt and Lawrence F. Shampine, 3-6-98
%   Copyright 1984-2000 The MathWorks, Inc. 
%   $Revision: 1.8 $
m1 = 0.1;
m2 = 0.1;
L = 1;
g = 9.81;
tspan = linspace(0,4,25);
y0 = [0; 4; 2; 20; -pi/2; 2];
options = odeset('Mass',@mass);
[t y] = ode45(@f,tspan,y0,options,m1,m2,L,g);
theta = y(1,5);
X = y(1,1);
Y = y(1,3);
xvals = [X X+L*cos(theta)];
yvals = [Y Y+L*sin(theta)];
figure;
%plot(xvals,yvals,xvals(1),yvals(1),'ro',xvals(2),yvals(2),'go')
plot(xvals,yvals,'-k',xvals(1),yvals(1),'-ko',    ...
     xvals(2),yvals(2),'-ks','MarkerSize',2,...
                             'MarkerEdgeColor','k',...
                             'MarkerFaceColor','k')
%title('A thrown baton problem with mass matrix M(t,y), solved by ODE45');
axis([0 22 0 25])
hold on
for j = 2:length(t)
  theta = y(j,5);
  X = y(j,1);
  Y = y(j,3);
  xvals = [X X+L*cos(theta)];
  yvals = [Y Y+L*sin(theta)];
% plot(xvals,yvals,xvals(1),yvals(1),'ro',xvals(2),yvals(2),'go')
  plot(xvals,yvals,'-k',xvals(1),yvals(1),'-ko',    ...
       xvals(2),yvals(2),'-ks','MarkerSize',2,...
                               'MarkerEdgeColor','k',...
                               'MarkerFaceColor','k')
end
hold off
% --------------------------------------------------------------------------
function dydt = f(t,y,m1,m2,L,g)
dydt = [
  y(2)
  m2*L*y(6)^2*cos(y(5))
  y(4)
  m2*L*y(6)^2*sin(y(5))-(m1+m2)*g
  y(6)
  -g*L*cos(y(5))
  ];

function M = mass(t,y,m1,m2,L,g)
M = zeros(6,6);
M(1,1) = 1;
M(2,2) = m1 + m2;
M(2,6) = -m2*L*sin(y(5));
M(3,3) = 1;
M(4,4) = m1 + m2;
M(4,6) = m2*L*cos(y(5));
M(5,5) = 1;
M(6,2) = -L*sin(y(5));
M(6,4) = L*cos(y(5));
M(6,6) = L^2;