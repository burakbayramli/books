% This script implements the weight-perturbation algorithm for the
% cart-pole swing-up. The policy is parameterized by the energy-based 
% swing-up controller, where the gains (Ke,Kp,Kd) are now learned.
% The control switches to lqr once the state is in its basin of attraction.
%
% The parameter vector is: alpha = [Ke Kp Kd]'.
% When implementing the parameter update use the learning rate (eta) given
% below.
% 
% Written by Rick Cory (recory@mit.edu)

function cartpole_wp
clear all;

% Parameters:
global mc mp l g;
global dt blqr;
mc = 1; mp = 1; l = 0.5; g = 9.8;

T = 10; % time horizon
dt = 1e-3; % simulation dt
disp_dt = 0.1; % draw dt
max_iter = 40; % maximum iterations

alpha = zeros(3,1);  % the parameters
eta = 0.1; % the (fixed) learning rate
IC = [0 0.1 0 0]'; % the initial conditions

% re-seed the random no. generator
rand('twister', sum(100*clock));

for iter=1:max_iter
    %   state is [xc,\theta,\dot{xc},\dot\theta]^T
    x = IC;

    % First run with non-perturbed weights
    last_display_t = -inf;
    xtraj = zeros(4,floor(T/dt)+1); % store the entire trajectory
    xtraj(:,1) = x; i = 1;
    for t=0:dt:T
        % draws the cartpole
        if (t>last_display_t + disp_dt)
            draw(x,t);
            last_display_t = t;
        end        
        
        % the control loop
        u = control(x,alpha,false);
        xdot = dynamics(x,u);
        x = x + dt*xdot;
        xtraj(:,i+1) = x; i=i+1;
    end
    
    % we are in the basin of attraction of lqr
    if (blqr)
        for t=T+dt:dt:T+10
            u = control(x,alpha,false);
            if (t>last_display_t + disp_dt)
                draw(x,t);
                last_display_t = t;
            end
            xdot = dynamics(x,u);
            x = x + dt*xdot;
        end
        disp('Success!');
        return;
    end
    
    % otherwise, we keep learning
    C = 0; % ENTER THE COST FOR THIS TRIAL
    disp(['Trial ',num2str(iter),'; Cost: ',num2str(C)]);    

    % run trial with perturbed weights
    sampling_interval = 6e-3;
    beta = sampling_interval*rand(3,1) - (sampling_interval/2); % the noise
    x = IC;
    xtraj(:,1) = x; i=1;
    for t=0:dt:T
        u = control(x,alpha+beta,true);
        xdot = dynamics(x,u);
        x = x + dt*xdot;
        xtraj(:,i+1) = x; i=i+1;
    end
    PC = 0; % ENTER THE PERTURBED COST FOR THIS TRIAL

    % IMPLEMENT THE WEIGHT PERTURBATION UPDATE HERE
    % (eta (given above) is the learning rate)
    
    %alpha = ?; 
    
end
end


% =============================================================
% Implements the Energy Swing-up controller 
% w/ learned gains (Ke,Kp,Kd)
% =============================================================
function u = control(x,z,blearn)
% state is [x,\theta,\dot{x},\dot\theta]^T 
global mc mp l g xdes blqr;
persistent K;

if (isempty(blqr))
    blqr = false;
    A = [
        0   0   1   0;
        0   0   0   1;
        0   mp*g/mc     0   0;
        0   g/l*(mp/mc + 1) 0   0];

    B = [0 0 1/mc   mc/l]';
    K = lqr(A,B,diag([10 1 1 1]), 1);
end

% switch to lqr
if (((abs(x(2)-pi) < pi/4) || blqr)&& ~blearn)
    blqr = true;
    xdes = [0 pi 0 0]';
    u = -K*(x - xdes);
    return;
end

% ** SET THE GAINS HERE **
Ke = 0; Kp = 0; Kd = 0;

s = sin(x(2)); c = cos(x(2));

% energy swing-up controller
E = 0.5*mp*l^2*x(4)^2 - mp*g*l*c;
Edes = mp*g*l;
E_tilde = E - Edes;

xddot_des = Ke*x(4)*c*E_tilde - Kp*x(1) - Kd*x(3);
u = (mc + mp*s^2)*xddot_des - mp*g*s*c - mp*l*x(4)^2*s;
end


% =============================================================
% Cart-pole dynamics
% =============================================================
function xdot = dynamics(x,u)
global mc mp l g;
s = sin(x(2)); c = cos(x(2));

xddot = (u + mp*s*(l*x(4)^2 + g*c))/(mc+mp*s^2);
tddot = (-u*c - mp*l*x(4)^2*c*s - (mc+mp)*g*s)/(l*(mc+mp*s^2));
xdot = [x(3:4); xddot; tddot];
end

% =============================================================
% Implements a final cost
% =============================================================
function C = finalCost(X)
Xdes = repmat([0 pi 0 0]',1,size(X,2)); % desired state

Xerr = (X-Xdes);
Q = diag([100 100 100 100]);
C = dot(Xerr,0.5*Q*Xerr);
end


% =============================================================
% The draw function
% =============================================================
function draw(x,t)
global l;
persistent hFig base a1 raarm wb lwheel;
if (isempty(hFig))
    hFig = figure(25); clf(25);
    set(hFig,'DoubleBuffer', 'on');

    a1 = l+0.25;
    av = pi*[0:.05:1];
    theta = pi*(0:0.05:2);
    wb = .3; hb=.15;
    aw = .01;
    wheelr = 0.05;
    lwheel = [-wb/2 + wheelr*cos(theta); -hb-wheelr + wheelr*sin(theta)]';
    base = [wb*[1 -1 -1 1]; hb*[1 1 -1 -1]]';
    arm = [aw*cos(av-pi/2) -a1+aw*cos(av+pi/2)
        aw*sin(av-pi/2) aw*sin(av+pi/2)]';
    raarm = [(arm(:,1).^2+arm(:,2).^2).^.5, atan2(arm(:,2),arm(:,1))];
end

figure(hFig); cla; hold on; view(0,90);
patch(x(1)+base(:,1), base(:,2),0*base(:,1),'b','FaceColor',[.3 .6 .4])
patch(x(1)+lwheel(:,1), lwheel(:,2), 0*lwheel(:,1),'k');
patch(x(1)+wb+lwheel(:,1), lwheel(:,2), 0*lwheel(:,1),'k');
patch(x(1)+raarm(:,1).*sin(raarm(:,2)+x(2)-pi),-raarm(:,1).*cos(raarm(:,2)+x(2)-pi), 1+0*raarm(:,1),'r','FaceColor',[.9 .1 0])
plot3(x(1)+l*sin(x(2)), -l*cos(x(2)),1, 'ko',...
    'MarkerSize',10,'MarkerFaceColor','b')
plot3(x(1),0,1.5,'k.')
title(['t = ', num2str(t,'%.2f') ' sec']);
set(gca,'XTick',[],'YTick',[])

axis image; axis([-2.5 2.5 -1.5*l 1.5*l]);
drawnow;
end

