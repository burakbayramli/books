close all; clear all; clc;
global g_accel Thrust2Weight
h = 185.2e3;
Vc = 1.627e3;
g_accel = 1.62;
Thrust2Weight = 3; % Thrust to Weight ratio for Ascent Vehicle, in lunar G's
rad2deg = 180/pi;

global x0 y0 Vx0 Vy0 yf Vxf Vyf
% pass these BCs to the BC function
% Initial conditions
% Launch from zero altitude with zero initial velocity
x0 = 0;
% meters, initial x-position
y0 = 0;
% meters, initial y-position
Vx0 = 0;
% m/s, initial downrange velocity
Vy0 = 0;
% m/s, initial vertical velocity
% Final conditions
yf = h;
% meters, final altitude
Vxf = Vc;
% m/s, final downrange velocity
Vyf = 0;
% m/s, final vertical velocity
%----------------------------------------------------------------------------
%% Initial Guesses
%----------------------------------------------------------------------------
% initial time
t0 = 0;
% list initial conditions in yinit, use zero if unknown
yinit = [x0 y0 Vx0 Vy0 0 0]; % guess for initial state and costate variables
tf_guess = 700;

Nt = 41;
tau = linspace(0,1,Nt)';
solinit = bvpinit(tau,yinit,tf_guess);

sol = bvp4c(@ascent_odes_tf, @ascent_bcs_tf, solinit);
% Extract the final time from the solution:
tf = sol.parameters(1);
% Evaluate the solution at all times in the nondimensional time vector tau
% and store the state variables in the matrix Z.
Z = deval(sol,tau);
% Convert back to dimensional time for plotting
time = t0 + tau.*(tf-t0);
% Extract the solution for each state variable from the matrix Z:
x_sol = Z(1,:);
y_sol = Z(2,:);
vx_sol = Z(3,:);
vy_sol = Z(4,:);
lambda2_bar_sol = Z(5,:);
lambda4_bar_sol = Z(6,:);

figure;
subplot(3,2,1);plot(time,x_sol/1000);
ylabel('x, km','fontsize',14);
xlabel('Time, sec','fontsize',14);
hold on; grid on;
title('Optimal Ascent from Flat Moon')
subplot(3,2,2);plot(time,y_sol/1000);
ylabel('y, km','fontsize',14);
xlabel('Time, sec','fontsize',14); hold on; grid on;

