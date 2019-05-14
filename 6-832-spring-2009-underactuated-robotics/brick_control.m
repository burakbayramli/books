function [tout,xout]=brick_control()
global A B K

IC=[]; %initial conditions

%use matlab 'ss' function to define the brick system using standard
%(the global variables are set up for standard x_dot=Ax+Bu notation)


%use matlab 'lqr' to find gain matrix (global variables set up for gain matrix to
%be K)

%simulate system
dt=.01;
T=20;
[tout,xout]=ode45(@control_dyn,[0:.01:T],IC);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%any additional code you need (e.g., plotting, etc)
%please place here

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plotdt=.1;
for i=1:plotdt/dt:T/dt
    draw((i-1)*dt,xout(i,:));
    pause(.001)
end

end

%dynamics function
function xdot=control_dyn(t,x)
global A B

u=lqr_control(x);
%u=minTime_control(x); %this controller may take a bit longer to simulate. that's fine

xdot=A*x+B*u;
end

%implement the LQR controller in this function
function u=lqr_control(x)
global K

end


%implement the minimum time controller in this function
function u=minTime_control(x)

end


% ==============================================================
% This is the draw function.
% ==============================================================
function draw(t,x)

persistent hFig blockx blocky;

if (isempty(hFig))
  hFig = figure(25);
  set(hFig,'DoubleBuffer','on');
  blockx = [-1, -1, 1, 1, -1];
  blocky = [0, 0.5, 0.5, 0, 0];
end

figure(hFig);
clf;

% draw the mass
brickcolor=[.75 .6 .5];
fill(blockx+repmat(x(1),1,5),blocky,brickcolor);
hold on

faintline=[.6 .8 .65]*1.1;
plot(min(blockx)+[0 0],[-5 5],'k:','Color',faintline);
plot(max(blockx)+[0 0],[-5 5],'k:','Color',faintline);

% draw the ground
line([-5, 5], [0, 0],'Color',[.3 .5 1],'LineWidth',1);
axis([-5 5 -1 2]);
%grid on
axis equal;
title(['t = ', num2str(t)]);

drawnow;
end