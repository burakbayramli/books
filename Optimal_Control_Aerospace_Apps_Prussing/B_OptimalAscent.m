close all; clear all; clc;
global g_accel Vc h eta beta f m0 mdot
h = 180000;
Vc = sqrt(3.9860044e5/(6378.14+h/1000))*1000;
g_accel = 9.80665; 
f = 2.1e6;
h_scale = 8440;
beta = h/h_scale;
rhoref = 1.225;
A = 7.069;

global xbar0 ybar0 Vxbar0 Vybar0 ybarf Vxbarf Vybarf
% Initial conditions
% Launch from zero altitude with zero initial velocity
% All Boundary Conditions are nondimensional
xbar0 = 0;
% initial x-position
ybar0 = 0;
% initial y-position
Vxbar0 = 0;
% initial downrange velocity
Vybar0 = 0;
% initial vertical velocity
% Final conditions
ybarf = h/h;
% final altitude
Vxbarf = Vc/Vc;
% final downrange velocity
Vybarf = 0;
% final vertical velocity
%% Parameters for the NO DRAG and CONSTANT MASS case
% Solve TPBVP without drag (C_D = 0) and constant mass (mdot = 0)
m0 = 60880;
% kg, average mass of a Titan II Rocket
CD = 0;
% Drag coefficient set to zero for no drag cases
mdot = 0;

eta = rhoref*CD*A/2;
%---------------------------------------------------------------------------
%% Initial Guesses
%---------------------------------------------------------------------------
% initial time
t0 = 0;
% list initial conditions in yinit, use zero if unknown
% guess for initial state and costate variables
yinit = [xbar0 ybar0 Vxbar0 Vybar0 0 -1 0];
tf_guess = 700;
% sec, initial guess for final time
% Because the final time is free, we must parameterize the problem by
% the unknown final time, tf. Create a nondimensional time vector,
% tau, with Nt linearly spaced elements. (tau = time/tf) We will pass the
% unknown final time to bvp4c as an unknown parameter and the code will
% attempt to solve for the actual final time as it solves our TPBVP.
% Nt is the number of points that the TPBVP will be discretized into. The
% larger Nt is, the more accurate your solution. However be aware that
% if Nt is too large, the solver may not be able to compute a solution
% using its algorithm.
Nt = 80;
tau = linspace(0,1,Nt)';
% nondimensional time vector
% Create an initial guess of the solution using the MATLAB function
% bvpinit, which requires as inputs the (nondimensional) time vector,
% initial states (or guesses, as applicable), and the guess for the final
% time. The initial guess of the solution is stored in the structure
% solinit.
solinit = bvpinit(tau,yinit,tf_guess);

sol = bvp4c(@B_ascent_odes_tf, @ascent_bcs_tf, solinit);
% Extract the final time from the solution:
tf = sol.parameters(1);
% Evaluate the solution at all times in the nondimensional time vector tau
% and store the state variables in the matrix Z.
Z = deval(sol,tau);
% Convert back to dimensional time for plotting
time = t0 + tau.*(tf-t0);
% Extract the solution for each state variable from the matrix Z and
% convert them back into dimensional units by multiplying each by their
% respective scaling constants.
x_sol = Z(1,:)*h/1000;
y_sol = Z(2,:)*h/1000;
vx_sol = Z(3,:)*Vc/1000;

vy_sol = Z(4,:)*Vc/1000;
lambda2_bar_sol = Z(5,:);
lambda3_bar_sol = Z(6,:);
lambda4_bar_sol = Z(7,:);
%% Parameters for VARYING MASS and NO DRAG case
m0 = 117020;
% Initial mass of Titan II rocket (kg)
mdot = (117020-4760)/139;
% Mass flow rate (kg/s)
delta_tf = 115;
% Amount subtracted from final time of constant mass
% case
%---------------------------------------------------------------------------
%% Solution for the VARYING MASS and NO DRAG case
%---------------------------------------------------------------------------
% Copy initial guess for the drag solution into a new structure named
% solinit_mass
solinit_mass = solinit;
% Save the time histories of the 7 state and costate variables from the NO
% DRAG, CONSTANT MASS solution in the structure of the initial guess for
% the VARYING MASS, NO DRAG case
solinit_mass.y = Z;
% Save the final time of the NO DRAG, CONSTANT MASS solution and use it as
% the guess for the final time for the VARYING MASS, NO DRAG case. Also
% subtract delta_tf from this guess as described before
solinit_mass.parameters(1) = tf-delta_tf;
% Run bvp4c for the VARYING MASS, NO DRAG
sol_mass = bvp4c(@ascent_odes_tf,@ascent_bcs_tf,solinit_mass);
% Evaluate the solution at all times in the nondimensional time vector tau
% and store the state variables in the matrix Z_mass.
Z_mass = deval(sol_mass,tau);
% Extract the final time from the solution with VARYING MASS, NO DRAG:
tf_mass = sol_mass.parameters(1);
% Convert back to dimensional time for plotting
time_mass = t0+tau*(tf_mass-t0);
% Extract the solution for each state variable from the matrix Z_mass and
% convert them back into dimensional units by multiplying each by their
% respective scaling constants.
x_sol_mass = Z_mass(1,:)*h/1000;
y_sol_mass = Z_mass(2,:)*h/1000;
vx_sol_mass = Z_mass(3,:)*Vc/1000;
vy_sol_mass = Z_mass(4,:)*Vc/1000;
lambda2_bar_sol_mass = Z_mass(5,:);
lambda3_bar_sol_mass = Z_mass(6,:);
lambda4_bar_sol_mass = Z_mass(7,:);

%% Parameters for the VARYING MASS AND DRAG case
CD = .5;
% Drag coefficient
eta = rhoref*CD*A/2;
% Update eta, since CD is now nonzero
%---------------------------------------------------------------------------
%% Solution for the VARYING MASS AND DRAG case
%---------------------------------------------------------------------------
% Copy initial guess for the VARYING MASS AND DRAG solution into a new
% structure named solinit_mass_drag
solinit_mass_drag = solinit_mass;

solinit_mass_drag.y = Z_mass;
% Save the final time of the VARYING MASS, NO DRAG solution and use it as
% the guess for the final time for the VARYING MASS AND DRAG case
solinit_mass_drag.parameters(1) = tf_mass;
% Run bvp4c for the drag case
sol_mass_drag = bvp4c(@ascent_odes_tf,@ascent_bcs_tf,solinit_mass_drag);
% Evaluate the solution at all times in the nondimensional time vector tau
% and store the state variables in the matrix Z_mass_drag.
Z_mass_drag = deval(sol_mass_drag,tau);
% Extract the final time from the solution:
tf_mass_drag = sol_mass_drag.parameters(1);
% Convert back to dimensional time for plotting
time_mass_drag = t0+tau*(tf_mass_drag-t0);
% Extract the solution for each state variable from the matrix Z_mass_drag
% and convert them back into dimensional units by multiplying each by their
% respective scaling constants.
x_sol_mass_drag = Z_mass_drag(1,:)*h/1000;
y_sol_mass_drag = Z_mass_drag(2,:)*h/1000;
vx_sol_mass_drag = Z_mass_drag(3,:)*Vc/1000;
vy_sol_mass_drag = Z_mass_drag(4,:)*Vc/1000;
lambda2_bar_sol_mass_drag = Z_mass_drag(5,:);
lambda3_bar_sol_mass_drag = Z_mass_drag(6,:);
lambda4_bar_sol_mass_drag = Z_mass_drag(7,:);

figure(1)
subplot(221)
plot(time_mass_drag,x_sol_mass_drag,'k')
xlabel('Time [s]','fontsize',14)
ylabel('x [km]','fontsize',14)
xlim([t0 tf_mass_drag])
subplot(222)
plot(time_mass_drag,y_sol_mass_drag,'k')
xlabel('Time [s]','fontsize',14)
ylabel('y [km]','fontsize',14)
xlim([t0 tf_mass_drag])
subplot(223)
plot(time_mass_drag,vx_sol_mass_drag,'k')
xlabel('Time [s]','fontsize',14)
ylabel('V_x [km/s]','fontsize',14)
xlim([t0 tf_mass_drag])
subplot(224)
plot(time_mass_drag,vy_sol_mass_drag,'k')
xlabel('Time [s]','fontsize',14)
ylabel('V_y [km/s]','fontsize',14)
xlim([t0 tf_mass_drag])

figure(2)
plot(time_mass_drag,...
atand(lambda4_bar_sol_mass_drag./lambda3_bar_sol_mass_drag),'k')
xlabel('Time [s]','fontsize',14)
ylabel('\theta [deg]','fontsize',14)
xlim([t0 tf_mass_drag])

figure(3)
plot(x_sol_mass_drag,y_sol_mass_drag,'k')
xlabel('Downrange Position, x [km]','fontsize',14)
ylabel('Altitude, y [km]','fontsize',14)
xlim([x_sol_mass_drag(1) x_sol_mass_drag(end)])
ylim([0 200])

					    
