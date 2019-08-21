clear all; close all; clc;
% Define parameters of the problem
% pass these parameters
global eta mdot Tbar tf
h0 = 300;
rearth = 6378;
mu = 3.986004418e5;
T = 20;
Isp = 6000;
mdot = T/Isp/9.80665;
m0 = 1500;

global r0bar u0bar v0bar theta0 ufbar % pass these BCs to the BC function
% Initial conditions
% Start from 300 km altitude circular orbit
% All Boundary Conditions are nondimensional
r0 = rearth+h0;
% Initial circular orbit radius [km]
u0 = 0;
% Initial radial component of velocity [km/s]
v0 = sqrt(mu/r0);
% Initial circular orbit speed [km/s]
theta0 = 0;
% Initial longitude [rad]
t0 = 0;
% Initial time [sec]
% Final conditions
uf = 0;
days = .1;
tf = 24*3600*days;

eta = v0*tf/r0;
% Scaled thrust Tbar
Tbar = T*tf/(v0*1000);
% Scaled Initial Conditions
r0bar = r0/r0;
u0bar = u0/v0;
v0bar = v0/v0;
% Scaled Final Conditions
ufbar = uf/v0;

Nt = 1000;
tau = linspace(0,1,Nt);
% nondimensional time vector
% list initial conditions in yinit
yinit = [r0bar u0bar v0bar 0 0 -1 0];
% Create an initial guess of the solution using the MATLAB function
% bvpinit, which requires as inputs the (nondimensional) time vector,
% initial states (or guesses, as applicable).
% The initial guess of the solution is stored in the structure solinit.
solinit = bvpinit(tau,yinit);
% Call bvp4c to solve the TPBVP. Point the solver to the functions
% containing the differential equations and the boundary conditions and
% provide it with the initial guess of the solution.
sol = bvp4c(@transfer_odes,@transfer_bcs,solinit);
% Evaluate the solution at all times in the nondimensional time vector tau
% and store the state variables in the matrix Z.
Z = deval(sol,tau);
% Implement for-loop to increment final time by 0.1 days each iteration
for days = .2:.1:2
  tf = 24*3600*days;
  Tbar = T*tf/(v0*1000);
  eta = v0*tf/r0;
  solinit.y = Z;
  solinit.x = tau;
  sol = bvp4c(@transfer_odes,@transfer_bcs,solinit);
  Z = deval(sol,tau);
end

for days = 2.05:.05:3.95
  tf = 24*3600*days;
  Tbar = T*tf/(v0*1000);
  eta = v0*tf/r0;
  solinit.y = Z;
  solinit.x = tau;
  sol = bvp4c(@transfer_odes,@transfer_bcs,solinit);
  Z = deval(sol,tau);  
end
days = 3.97152;
tf = 24*3600*days;
Tbar = T*tf/(v0*1000);
eta = v0*tf/r0;
solinit.y = Z;
solinit.x = tau;
sol = bvp4c(@transfer_odes,@transfer_bcs,solinit);
Z = deval(sol,tau);

tau2 = linspace(0,1,5*Nt);
for q = 1:7
Z2(q,:) = spline(tau,Z(q,:),tau2);
end
tau = tau2;
Z = Z2;
solinit.y = Z;
solinit.x = tau;
sol = bvp4c(@transfer_odes,@transfer_bcs,solinit);
Z = deval(sol,tau);
time = t0+tau*(tf-t0);

r_sol = Z(1,:)*r0;
u_sol = Z(2,:)*v0;
v_sol = Z(3,:)*v0;
theta_sol = Z(4,:);
lambda_rbar_sol = Z(5,:);

lambda_ubar_sol = Z(6,:);
lambda_vbar_sol = Z(7,:);
245
% 2nd costate
% 3rd costate
% Displays final value of orbit radius
final_radius = r_sol(end)
%---------------------------------------------------------------------------
%% Plots
%---------------------------------------------------------------------------
figure(1)
plot(r_sol.*cos(theta_sol), r_sol.*sin(theta_sol),'k')
xlabel('x-direction [km]')
ylabel('y-direction [km]')
axis equal
% Plot the steering angle as a function of time
figure(2)
plot(time/(3600*24), atand(lambda_ubar_sol./lambda_vbar_sol),'k')
xlabel('Time [days]')
ylabel('\theta(t) [deg]')
xlim([time(1) time(end)]/(3600*24))








