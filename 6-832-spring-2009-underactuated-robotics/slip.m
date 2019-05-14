function slip

% Spring Loaded Inverted Pendulum
%
% This code was written by Russ Tedrake <russt@mit.edu>.
% Modified for 6.832 problem set by Rick Cory


global m r0 theta0 k g plant_dt display_dt yd;

m = 1;
r0 = 1;
% this is the touchdown angle for the passive system
theta0 = pi/8;
k = 300;
g = 9.8;
plant_dt = 1e-3;
display_dt = 0.005;
y0 = 1.5;
yd = 1.5;
E = 30;

% simulate the passive system
simulate(y0,E);

end


% ================================================================ 
% Simulates and draws the system. 
% YOU'LL NEED TO ADD CODE FOR THE DEADBEAT CONTROLLER. 
% ===============================================================
function simulate(y,E)

global m r0 theta0 k g plant_dt display_dt;

%theta0 = .4; % this is a fixed touch down angle
x = apex_to_state(y,E);
t = 0; last_display_t = -inf;

while (1)
  % x(5) below is just an indicator that tells you what mode you're in,
  % i.e. flight, flight-to-stance, stance-to-flight
  if (x(5))
    x = x + plant_dt*flight_dynamics(x);
    if (x(2)<r0*cos(theta0) && x(4)<0)
      x = flight_to_stance(x);
    end
  else
    x = x + plant_dt*stance_dynamics(x);
    if (x(1)>r0 && x(3)>0)
      x = stance_to_flight(x);
      % check for falling
      if (x(3)<0 || x(4)<0), break; end
    end
  end
  t = t+plant_dt;
  if (t-last_display_t>display_dt)
    draw(x,t);
    last_display_t = t;
  end
end

end

% === Returns the state for a given apex and energy ======
function x = apex_to_state(y,E)

global m r0 theta0 k g;

x = [0; y; sqrt(2/m*(E - m*g*y)); 0; 1];

end

% === Flight-to-stance dynamics ===================
function xp = flight_to_stance(x)
% x = [x;y;xdot;ydot]
% xp = [r,theta,rdot,thetadot]

global m r0 theta0 k g;

r = x(2)/cos(theta0);

xp = [ r ; theta0; ...
  -x(3)*sin(theta0) + x(4)*cos(theta0); ...
  -(x(3)*cos(theta0) + x(4)*sin(theta0))/r; ...
  0];
end

% === Stance-to-flight dynamics ================
function x = stance_to_flight(xp)
% x = [x;y;xdot;ydot]
% xp = [r,theta,rdot,thetadot]

global m r0 theta0 k g;

x = [ -xp(1)*sin(xp(2));...
  xp(1)*cos(xp(2)); ...
  -xp(3)*sin(xp(2)) - xp(1)*xp(4)*cos(xp(2)); ...
  xp(3)*cos(xp(2)) - xp(1)*xp(4)*sin(xp(2)); ...
  1];

end
  
% === Flight dynamics ================================
function xdot = flight_dynamics(x)
% x = [x;y;xdot;ydot]

global m r0 theta0 k g;

xdot = [x(3:4); 0; -g/m; 0];

end

% === Stance dynamics ===============================
function xpdot = stance_dynamics(xp)
% xp = [r,theta,rdot,thetadot]

global m r0 theta0 k g;

xpdot = [xp(3:4); ...
  k/m*(r0-xp(1)) + xp(1)*xp(4)^2 - g*cos(xp(2)); ... 
  (g*xp(1)*sin(xp(2)) - 2*xp(1)*xp(3)*xp(4))/(xp(1)^2); ...
  0];
  
end

% ==== The draw function =======================================
function draw(x,t)

global m r0 theta0 k g;
figure(25);
set(gcf,'DoubleBuffer','on');

cla;
hold on;

if (x(5)>0)
  hip = x(1:2);
  toe = x(1:2) + r0*[sin(theta0); -cos(theta0)];
else
  hip = [-x(1)*sin(x(2)); x(1)*cos(x(2))];
  toe = [0;0];
end

if (nargin>1)
  title(['t = ', num2str(t)]);
end

line([hip(1);toe(1)], [hip(2); toe(2)],'Color',[0 0 0],'LineWidth',2);

t = 0:0.1:2*pi;
line(hip(1)+0.15*sin(t),hip(2)+0.15*cos(t),'Color',[0 0 0]);
fill(hip(1)+0.15*sin(t),hip(2)+0.15*cos(t),[ 0.502 1.000 1.000 ]);

line([-4,4],[0,0]);
axis equal;

drawnow;
end