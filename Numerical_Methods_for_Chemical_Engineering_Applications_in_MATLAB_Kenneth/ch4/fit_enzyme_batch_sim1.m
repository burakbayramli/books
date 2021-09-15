% fit_enzyme_batch_sim1.m
% This program fits the rate law from
% batch kinetic data of an enzymatic
% reaction.
% Kenneth J. Beers
% MIT Department of Chemical Engineering
% May 23, 2005

function [V_m,K_m,K_si,iflag] = fit_enzyme_batch_sim1();
iflag = 0;

% set kinetic data
Data.time_min = [30; 60; 90; 120; 180; 240; 300; 315; 330];
Data.S_M = [1.87; 1.73; 1.58; 1.43; 1.07; 0.63; 0.12; 0.04; 0.01];

% plot data
figure;  plot(Data.time_min,Data.S_M,'o');
xlabel('time (min)');  ylabel('[S] (M)');
title('Measured substrate concentration vs. time');

% ask user for initial guesses of parameters
V_m_0 = input('Enter guess for V_m : ');
K_m_0 = input('Enter guess for K_m : ');
K_si_0 = input('Enter guess for K_si : ');
theta_0 = [V_m_0; K_m_0; K_si_0];

% call minimizer to fit rate law to data
Options = optimset('Display','iter');
imethod = input('Use fminsearch (0) or fminunc (1) ? : ');
if(~imethod)
    theta = fminsearch(@calc_SSE,theta_0, Options, Data);
else
    Options = optimset('LargeScale','off');
    theta = fminunc(@calc_SSE, theta_0, Options, Data);
end

% extract results
V_m = theta(1);  K_m = theta(2);  K_si = theta(3);

% compute results with fitted parameters
Sys.V_m = theta(1);  Sys.K_m = theta(2);
Sys.K_si = theta(3);
Sys.m_E_mg = 10;  Sys.V_R_mL = 100;
S_0 = 2;  % initial molar concentration
% compute substrate concentrations
t_plot = linspace(0,max(Data.time_min),100)';
S_plot = enzyme_batch_sim1(Sys,t_plot,S_0);
% make plot
figure;  plot(Data.time_min,Data.S_M,'o');
hold on;  plot(t_plot,S_plot);
xlabel('time (min)');  ylabel('[S] (M)');
title('Measured substrate concentration vs. time');

iflag = 1;
return;


% ------------------------------------
% This routine returns the cost function value used
% to fit the enzyme batch kinetic data using the
% least-squares method.
function SSE = calc_SSE(theta,Data);

% set system parameters
Sys.V_m = theta(1);  Sys.K_m = theta(2);
Sys.K_si = theta(3);
Sys.m_E_mg = 10;  Sys.V_R_mL = 100;
S_0 = 2;  % initial molar concentration

% compute substrate concentrations
S = enzyme_batch_sim1(Sys,Data.time_min,S_0);

% compute sum of squared errors cost function
res = Data.S_M - S;
SSE = 0.5*dot(res,res);

return;


% ----------------------------------------
% This routine computes the substrate concentration
% as a function of time for batch kinetics showing
% substrate inhibition.
%
% Input Data:
% -----------
% Sys : a structure containing the system parameters
%   .V_m : max. rate in micromoles/min/mg of enzyme
%   .K_m : equil. constant in M
%   .K_si : inhibition equil. constant in M
%   .m_E_mg : mass of enzyme in mg
%   .V_R_mL : reactor volume in mL
% time : array of times at which measurement is taken
% S_0 : initial substrate concentration in M
%
% Output Data :
% -------------
% S : array of substrate concentrations (in M) at
%     time values specified in time input argument
%
% Kenneth J. Beers
% MIT Department of Chemical Engineering
% May 23, 2005

function [S,iflag] = enzyme_batch_sim1(Sys,time,S_0);
iflag = 0;

% perform dynamic simulation to compute
% substrate concentrations at desired times
if(time(1) ~= 0)
    t_report = [0; time];
else
    t_report = time;
end
[t_traj,S_traj] = ode45(@enzyme_batch,t_report,S_0,[],Sys);

if(time(1) ~= 0)
    S = S_traj(2:(length(time)+1));
else
    S = S_traj;
end

iflag = 1;
return;


% --------------------
% This routine returns the time derivative
% of the substrate concentration.
function dS_dt = enzyme_batch(t,S,Sys);

V_R = Sys.V_R_mL/1000;  % reactor volume in L
alpha_c = 1e6;  % micromoles per mole
var1 = Sys.m_E_mg/alpha_c/V_R;

rate = (Sys.V_m.*S)./(Sys.K_m + S + (S.^2)./Sys.K_si);

dS_dt = -var1.*rate;

return;

