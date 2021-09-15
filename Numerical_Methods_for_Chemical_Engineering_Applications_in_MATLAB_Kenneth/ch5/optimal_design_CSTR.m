% optimal_design_CSTR.m
% This program optimizes the design of a CSTR
% that produces C from A and B through the network:
% A + B --> C   r_R1 = k1*cA*cB
% A --> S1  r_R2 = k2*cA
% C --> S2  r_R3 = k3*cC
% The design is optimized to maximize the production
% of C from A.
% Kenneth Beers.
% MIT Department of Chemical Engineering
% May 29, 2005

function iflag_main = optimal_design_CSTR();
iflag_main = 0;
disp(' '); disp('Running optimal design_CSTR ...');

% ask user for choice of design goal
imode = input(...
    'Enter 1 for max. C production, 2 for max. economic benefit : ');
prices = zeros(5,1);
if(imode == 2)
    prices(1) = input('Enter $ per mol. for A : ');
    prices(2) = input('Enter $ per mol. for B : ');
    prices(3) = input('Enter $ per mol. for C : ');
    prices(4) = input('Enter $ per mol. to dispose S1 : ');
    prices(5) = input('Enter $ per mol. to dispose S2 : ');
    disp(' ');
end

% ask user for initial guesses of parameters
volFlow_g = input('Enter guess of volFlow (L per hr) : ');
cA_0_g = input('Enter guess of cA_0 (M) : ');
cB_0_g = input('Enter guess of cB_0 (M) : ');
T_g = input('Enter guess of T (K) : ');
theta_g = [volFlow_g; cA_0_g; cB_0_g; T_g];

% compute initial cost function value
F_g = CSTR_cost_fcn(theta_g, imode, prices);
disp(' '); disp(['  initial cost fcn = ', num2str(F_g)]);

% perform constrained optimization
% lower and upper bounds
LB = [1e-4; 1e-4; 1e-4; 298];  % lower bounds
UB = [360; 2; 2; 360];  % upper bounds
% linear inequality constraint, cA_0 + cB_0 <= 2
A = [0 1 1 0];  b = 2;
% set options and call fmincon
Options = optimset('LargeScale','off','Display','iter');
[theta,F,exitflag,output] = fmincon(@CSTR_cost_fcn, ...
    theta_g, A, b, [], [], LB, UB, [], Options, imode, prices);
% extract and report results
disp(' '); exitflag, output,
disp('Optimized design : '); F,
volFlow = theta(1),  cA_0 = theta(2),
cB_0 = theta(3), T = theta(4),
% compute final state and report results
[F,x] = CSTR_cost_fcn(theta,imode,prices);
disp(' ');
cA = x(1),  cB = x(2),  cC = x(3),  cS1 = x(4),  cS2 = x(5),

iflag_main = 1;
return;


% --------------------
% This routine computes the cost function for the
% reactor by computing the stable steady-state
% reactor concentrations through dynamic integration.
% As a second argument, the routine returns the final
% state vector at steady state.
function [F,x] = CSTR_cost_fcn(theta, imode, prices);

% extract the design parameters
volFlow = theta(1);  cA_0 = theta(2);
cB_0 = theta(3);  T = theta(4);

% perform a dynamic integration
x0 = [cA_0; cB_0; 0; 0; 0];  % initial state
% integrate in multiples of the reactor residence time
% until the time derivatives are less than a tolerance value
V_R = 1;  res_time = V_R/volFlow;
for iter=1:1000
    [t_traj,x_traj] = ode23s(@CSTR_xdot,[0 2*res_time],x0,[],...
        volFlow, cA_0, cB_0, T, V_R);
    % extract final state
    num_traj = length(t_traj);  x = x_traj(num_traj,:)';
    % check if we have approached steady-state
    xdot = CSTR_xdot(t_traj(num_traj),x,volFlow,cA_0,cB_0,T,V_R);
    xdot_norm = norm(xdot,inf);
    if(xdot_norm <= 1e-6)
        break;
    else
        x0 = x;
    end
end
% extract final values of state variables
cA = x(1);  cB = x(2);  cC = x(3);  cS1 = x(4);  cS2 = x(5);

% if maximizing production of C
if(imode == 1)
    F = -volFlow*cC;
else  % maximize net economic benefit
    F = -volFlow*(-prices(1)*(cA_0-cA) - prices(2)*(cB_0-cB) ...
        + prices(3)*cC - prices(4)*cS1 - prices(5)*cS2);
end

return;

% ----------
% This routine returns the time derivatives for the CSTR.
function xdot = CSTR_xdot(t,x,volFlow,cA_0,cB_0,T,V_R);
xdot = zeros(5,1);
cA = x(1);  cB = x(2);  cC = x(3);  cS1 = x(4);  cS2 = x(5);

var1 = volFlow/V_R;  % inverse residence time
% compute reaction rates
k1 = 2.3*exp(15.4*(1-298/T));  r_R1 = k1*cA*cB;
k2 = 0.2*exp(17.9*(1-298/T));  r_R2 = k2*cA;
k3 = 0.1*exp(22.3*(1-298/T));  r_R3 = k3*cC;

% compute time derivatives
xdot(1) = var1*(cA_0 - cA) - r_R1 - r_R2;  % A balance
xdot(2) = var1*(cB_0 - cB) - r_R1;  % B balance
xdot(3) = -var1*cC + r_R1 - r_R3;  % C balance
xdot(4) = -var1*cS1 + r_R2;  % S1 balance
xdot(5) = -var1*cS2 + r_R3;  % S2 balance

return;

