function [J,dJdalpha] = pendfun(alpha_in)

% dynamics dt
dt = 0.01; T = 2.5;

% pendulum parameters
global m g l I b xdes;
m=1; g = 9.8; l = 1; I = m*l*l; b = 0.1;
xdes = [pi 0]'; % the desired final state

N = floor(T/dt)+1;
xtape = zeros(2,N);
utape = zeros(1,N);
alpha = zeros(N,1);
if nargin>0
    alpha = alpha_in;
end

% Simulate forward
IC = [0 0]';
x = IC; % arbitrary (but fixed) initial condition
for i=1:N
    xtape(:,i) = x;
    u = alpha(i);
    utape(i) = u;
    x = x + dynamics(x,u).*dt;
end

figure(24)
plot(xtape(1,:),xtape(2,:)); drawnow;

dJdalpha = compute_gradients(xtape,utape,dt);
J = sum(cost(xtape,utape,dt)) + finalCost(xtape(:,N));

end % of pendfun


% =========================================================
% This function returns the gradients by
% integrating the adjoint equations
% =========================================================
function dJdalpha = compute_gradients(x,u,dt)
global xdes; %desired x location
N = size(x,2);

[Q,R,Qend] = get_QR;
Q = Q.*dt; R = R.*dt;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%FILL IN THIS SECTION TO INTEGRATE ADJOINT EQUATIONS TO PERFORM BPTT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[dfdx,dfdu] = gradients(x(:,N),u(N)); 
dgdu = u(N)*R;
dudalpha = zeros(1,N); dudalpha(N) = 1; %gradient of u w.r.t. parameters for open loop policy at N
F_alpha = dfdu*dudalpha;
G_alpha = dgdu*dudalpha;

y = ; %give terminal condition for y
dJdalpha = (G_alpha'-F_alpha'*y).*dt; % dJdalpha for first time step
for n=N-1:-1:1 %integrate adjoint equations backwards in time
    dgdx = ; %gradient of cost with respect to current state 
    dgdu = ; %gradient of cost with respect to current action
    [dfdx,dfdu] = gradients(x(:,n),u(n)); %gradient of f w.r.t. current position, action
    F_x = dfdx;
    G_x = dgdx;
    dudalpha = zeros(1,N); dudalpha(n) = 1; %gradient of u w.r.t. parameters for open loop policy at current time
    F_alpha = dfdu*dudalpha;
    G_alpha = dgdu*dudalpha;
    y = ; %solve for y
    dJdalpha = ; %add this step's contribution to dJdalpha
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end

% ============================================================
% This function evaulates the gradients at a particular x,u
% ============================================================
function [dfdx,dfdu] = gradients(x,u)
% pend parameters
global m g l I b;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Give the gradients of the dynamics with respect to 
% a particular state and action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dfdx = ;
dfdu = ;

end

% =============================================================
% This function defines the continuous dynamics of the pendulum
% =============================================================
function xdot = dynamics(x,u)
% pendulum parameters
global m g l I b;

xdot = [x(2,:); (u-m*g*l*sin(x(1,:))-b*x(2,:))./I];
end


% =============================================================
% This function defines the instantaneous cost (i.e. g(x,u))
% =============================================================
function C = cost(X,u,dt)
global xdes;


Xerr = X - repmat(xdes,1,size(X,2));
Xerr(1,:) = mod(Xerr(1,:)+pi,2*pi)-pi;

% implement a quadratic cost

C = ;
end

% =============================================================
% Implements a final cost
% =============================================================
function C = finalCost(X)
global xdes;

Xerr = X - repmat(xdes,1,size(X,2));
Xerr(1,:) = mod(Xerr(1,:)+pi,2*pi)-pi;

% implement the final cost cost

C = ;
end

% ============================================================
% Returns the cost matrices
% ============================================================
function [Q,R,Qend] = get_QR

% penalty matrices

end