% PBR_DAE_sim.m
% This program simulates the steady-state profile
% in a Packed Bed Reactor (PBR) of the reaction
% A <==> B + C, occuring on a solid catalyst in
% contact with a gas stream of A, B, C, and a
% non-reactive dilutent gas G. The reactor is assumed
% to be isothermal and there are no mass transfer
% limitations. Surface reaction is assumed to be
% rate limiting with adsorption/desorption in
% equilibrium.
% Written by Kenneth J. Beers.
% MIT Department of Chemical Engineering
% April 13, 2005

function iflag_main = PBR_DAE_sim();
iflag_main = 0;

% First, set the rate law parameter
Rate = set_Rate();

% Next, set reactor parameters
Reactor = set_Reactor();

% set initial state vector
x_0 = set_initial_state(Rate,Reactor);

% set mass matrix
Mass = zeros(7,7);
Mass(1:4,1:4) = eye(4);

% set Options structure
Options = odeset('Mass',Mass,'MassSingular','yes', ...
    'MStateDependence','none');

% ask user to input mass of catalyst in reactor
W_tot = input('Enter tot. cat. mass in PBR : ');

% perform simulation
[W_traj, x_traj] = ode15s(@PBR_calc_f, [0 W_tot], ...
    x_0, Options, Rate, Reactor);

% plot results
plot_results(W_traj,x_traj,Rate,Reactor);

iflag_main = 1;
return;


% ------------------------------
% This routine sets the rate law parameters.
function Rate = set_Rate();

% set reaction rate law parameters
% moles of active sites per Kg of catalyst
Rate.c_t = 1e-3;
% adsorption equilibrium constant for A
Rate.Ka_A = 4.2e-5;  % in 1/Pa
% adsorption equilibrium constant for B
Rate.Ka_B = 2.5e-5;  % in 1/Pa
% forward surface reaction rate constant
Rate.k_s = 30;  % in 1/s
% equilibrium constant for surface reaction
Rate.Keq_s = 9.12e5;  % in Pa

return;


% ------------------------------
% This routine sets the reactor parameters.
function Reactor = set_Reactor();

% diameter of catalyst particles
Reactor.D_p = 0.005;  % in m
% void fraction of bed
Reactor.phi = 0.64;
% cross sectional area of bed
Reactor.A_c = 0.0079;  % in m^2
% density of solid catalyst phase
Reactor.rho_s = 900; % in Kg/m^3

% set inlet gas properties
% total pressure of inlet stream
Reactor.P_0 = 101325;  % in Pa
% partial pressure of A in inlet stream
Reactor.p_A0 = 0.1*101325; % in Pa
% inlet volumetric flow rate
Reactor.vflow_0 = 0.001; % in m^3/s
% inlet temperature
Reactor.T_0 = 373;  % in K
% molar flow rate of A in feed stream
Reactor.F_A0 = Reactor.p_A0*Reactor.vflow_0 ...
    / 8.314 / Reactor.T_0;  % in mol/s
% molar flow rate of dilutent gas in feed stream
Reactor.F_G0 = (Reactor.P_0 - Reactor.p_A0) ...
    * Reactor.vflow_0/8.314/Reactor.T_0; % in mol/s
% total molar flow rate of feed stream in mol/s
Reactor.F_tot0 = Reactor.F_A0 + Reactor.F_G0;
% density of gas phase at inlet conditions
Reactor.rho_0 = 2.9;  % in Kg/m^3
% viscosity of gas pahse
Reactor.mu = 2e-5;  % in Pa*s
% superficial mass velocity
Reactor.gamma = Reactor.rho_0 ...
    *Reactor.vflow_0/Reactor.A_c;
% Ergun constant beta_0 for model of pressure
% drop across packed bed, assumed constant viscosity
var1 = 150*(1-Reactor.phi)*Reactor.mu/Reactor.D_p;
var2 = Reactor.gamma*(1-Reactor.phi) ...
    /Reactor.rho_0/Reactor.D_p/(Reactor.phi^3);
Reactor.beta_0 = var2*(var1 + 1.75*Reactor.gamma);

return;


% ------------------------------
% This routine initializes the state of
% the system to the inlet conditions.
function x_0 = set_initial_state(Rate,Reactor);

x_0 = zeros(7,1);  % allocate memory

R = 8.314;  % ideal gas constant in SI units
x_0(1) = Reactor.F_A0;  % molar flow rate of A
x_0(2) = 0;  % molar flow rate of B
x_0(3) = 0;  % molar flow rate of C
x_0(4) = Reactor.P_0;  % pressure
% following values from isotherm of A
c_v = Rate.c_t/(1+Rate.Ka_A*Reactor.p_A0);
c_AS = Rate.Ka_A*Reactor.p_A0*c_v;
x_0(5) = c_AS;  % conc. of absorbed A
x_0(6) = 0;  % conc. of absorbed B
x_0(7) = c_v;  % conc. of vacant sites

return;


% ------------------------------
% This routine returns the function vector for
% the PBR model
function f = PBR_calc_f(W, x, Rate, Reactor);

f = zeros(7,1);

% extract state variables
F_A = x(1);  % molar flow rate of A
F_B = x(2);  % molar flow rate of B
F_C = x(3);  % molar flow rate of C
P = x(4);  % total pressure
c_AS = x(5);  % conc. of absorbed A
c_BS = x(6);  % conc. of adsorbed B
c_v = x(7);  % conc. of vacant sites

% compute total molar flow rate
F_tot = F_A + F_B + F_C + Reactor.F_G0;

% compute partial pressures in gas phase
p_A = P*F_A/F_tot;
p_B = P*F_B/F_tot;
p_C = P*F_C/F_tot;

% compute volumetric flow rate in m^3/s
R = 8.314;  % ideal gas constant in SI units
T = Reactor.T_0;  % isothermal operation
vflow = F_tot*R*T/P;  % volumetric flow rate

% compute surface reaction rate
r_R = Rate.k_s*(c_AS - c_BS*p_C/Rate.Keq_s);

% Next, evaluate derivative functions.

% mole balance on A
f(1) = -r_R;

% mole balance on B
f(2) = r_R;

% mole balance on C
f(3) = r_R;

% pressure drop across packed bed
var1 = Reactor.beta_0/Reactor.A_c ...
    /(1-Reactor.phi)/Reactor.rho_s;
f(4) = -var1*(Reactor.P_0/P)...
    *(F_tot/Reactor.F_tot0);

% adsorption equilibrium of A
f(5) = Rate.Ka_A*p_A*c_v - c_AS;

% adsorption equilibrium of B
f(6) = Rate.Ka_B*p_B*c_v - c_BS;

% site balance
f(7) = Rate.c_t - c_v - c_AS - c_BS;

return;


% ------------------------------
% This routine plots the results
function iflag = plot_results(...
    W_traj,x_traj,Rate,Reactor);
iflag = 0;

figure;

% first plot is of molar flow rates
subplot(2,1,1);
plot(W_traj,x_traj(:,1));  % F_A
hold on;
plot(W_traj,x_traj(:,2),'--');  % F_B
plot(W_traj,x_traj(:,3),'-.');  % F_C
xlabel('W (Kg)');
ylabel('F_j (mol/s)');
title('Molar flow rates');
legend('A','B','C','Location','Best');

% second plot is of catalyst concentrations
subplot(2,1,2);
plot(W_traj,x_traj(:,5));  % c_AS
hold on;
plot(W_traj,x_traj(:,6),'--');  % c_BS
plot(W_traj,x_traj(:,7),'-.');  % c_v
xlabel('W (Kg)');
ylabel('c_j (mol/Kg-cat)');
title('catalyst concentrations');
legend('AS','BS','v','Location','Best');

iflag = 1;
return;




