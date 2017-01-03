% Demo routine: interactive simulation and numerical solution of Ito and Stratonovich SDEs.
%
% usage: SDE_demo;

% Copyright (C) 2007, Umberto Picchini  
% umberto.picchini@biomatematica.it
% http://www.biomatematica.it/Pages/Picchini.html
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.


fprintf('\n\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::  SDE TOOLBOX  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n');
fprintf('                             SIMULATION AND ESTIMATION OF STOCHASTIC DIFFERENTIAL EQUATIONS WITH MATLAB   \n\n');
fprintf('                                                  http://sdetoolbox.sourceforge.net                                                 \n');
fprintf('                                                                                                                                    \n');
fprintf('                                                  Copyright (C) 2007, Umberto Picchini                                              \n');
fprintf('                                                  umberto.picchini@biomatematica.it                                                 \n');
fprintf('                                                  http://www.biomatematica.it/pages/picchini.html                                 \n\n');
fprintf(' This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License.       \n\n');
fprintf(':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');

global PROBLEM OWNTIME NUMSIM NUMDEPVARS SDETYPE DW;

ok = input('\n\nPress any key to continue...');

fprintf('\n\n:::::::::::::::::::::::::::::::::::::::::::::::::::   DEMO   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
fprintf('\n\n In this demo we simulate several trajectories of the actual and numerical solution of the following SDE:');
fprintf('\n\n (Ito SDE)     dXt = 1/2 * a^2 * Xt * dt + a * Xt * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\n (Stratonovich SDE)     dXt = a * Xt o dWt,    X(0) = X0');
fprintf('\n\n into the time interval [T0, T] with fixed integration stepsize h.');
fprintf('\n\n The SDE has analytic solution given by:     Xt = X0 * exp(a * Wt)');
fprintf('\n\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');

ok = input('\n\nPress any key to continue...');






% NOW LET MATLAB DO THE JOB...

bigtheta = SDE_demo_interactive_settings; % bigtheta contains the structural SDE model parameters
Xzero = bigtheta(1) ;  
a = bigtheta(2); 


%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                      ITO SDE COMPUTATIONS
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SDETYPE = 'Ito';   % specifies the SDE type (Ito or Strat (Stratonovich))

N = length(OWNTIME);

% preallocate matrices for efficiency
xTrue = zeros(N,NUMSIM*NUMDEPVARS);   
xItoEuler = zeros(N,NUMSIM*NUMDEPVARS);  
xItoMilstein = zeros(N,NUMSIM*NUMDEPVARS);  
errorT_ItoMilstein = zeros(NUMSIM,NUMDEPVARS);
errorT_ItoEuler = zeros(NUMSIM,NUMDEPVARS);
lbxItoEuler = zeros(NUMSIM,NUMDEPVARS);
ubxItoEuler = zeros(N,NUMDEPVARS);
q1xItoEuler = zeros(N,NUMDEPVARS);
q3xItoEuler = zeros(N,NUMDEPVARS);
meanxItoEuler = zeros(N,NUMDEPVARS);
lbxItoMilstein = zeros(N,NUMDEPVARS);
ubxItoMilstein = zeros(N,NUMDEPVARS);
q1xItoMilstein = zeros(N,NUMDEPVARS);
q3xItoMilstein = zeros(N,NUMDEPVARS);
meanxItoMilstein = zeros(N,NUMDEPVARS);

%::: computing trajectories ::::::::::::

W = cumsum(DW,1);                                           % the Brownian paths
xTrue = Xzero*exp(a*W);                                     % the true (analytic) solution of the (Ito or Stratonovich) SDE 
xItoMilstein = SDE_milstein_demo(bigtheta);                 % the Milstein solution of the Ito SDE, COMPUTED ON THE SAME BROWNIAN PATHS OF THE TRUE SOLUTION
xItoEuler = SDE_euler_demo(bigtheta);                       % the Euler-Maruyama solution of the Ito SDE, COMPUTED ON THE SAME BROWNIAN PATHS OF THE TRUE SOLUTION AND THE MILSTEIN APPROXIMATION
errorT_ItoMilstein=abs(xItoMilstein(end,:)-xTrue(end,:));   % the absolute error with Milstein (Ito) solution at time T 
errorT_ItoEuler=abs(xItoEuler(end,:)-xTrue(end,:));         % the absolute error with Euler-Maruyama (Ito) solution at time T 

% plot graphs
handle = waitbar(0,'Ito SDE: computing statistics and plotting simulations...');
figure
plot(OWNTIME,xItoMilstein,'k:'), hold on
waitbar(1/15);
plot(OWNTIME,xItoEuler,'k-'),
waitbar(2/15);
plot(OWNTIME,xTrue,'m-'), hold off
waitbar(3/15);
xlabel('t','Fontsize',13,'Rotation',0);
ylabel('X_t','Fontsize',13,'Rotation',0);
titlestring = sprintf('Ito SDE: Euler-Maruyama vs Milstein vs analytic solution over %d trajectories',NUMSIM);
title(titlestring);


% Now compute mean, 95% confidence intervals and q1-q3 quartiles for the Euler-Maruyama approximation
figure
lbxItoEuler = perctile(xItoEuler',2.5); % Lower Bound of the 95% confidence area obtained by taking, at each time, the 2.5% percentile of the simulated trajectories
waitbar(4/15);
ubxItoEuler = perctile(xItoEuler',97.5); % Upper Bound of the 95% confidence area obtained by taking, at each time, the 97.5% percentile of the simulated trajectories
waitbar(5/15);
q1xItoEuler = perctile(xItoEuler',25); % first quartile of the simulated trajectories
waitbar(6/15);
q3xItoEuler = perctile(xItoEuler',75); % third quartile of the simulated trajectories
waitbar(7/15);
meanxItoEuler = mean(xItoEuler,2);
waitbar(8/15);
%                 the empirical 95% CI,                                 first and third quartile              the process empirical mean
plot(OWNTIME,lbxItoEuler,'k--',OWNTIME,ubxItoEuler,'k--',OWNTIME,q1xItoEuler,'k:',OWNTIME,q3xItoEuler,'k:',   OWNTIME,meanxItoEuler,'g-') 
waitbar(9/15);
xlabel('t','Fontsize',13,'Rotation',0);
ylabel('X_t','Fontsize',13,'Rotation',0);
titlestring = sprintf('Ito SDE: mean, 95 percent CI, q1-q3 quartiles of the Euler-Maruyama approximation over %d trajectories',NUMSIM);
title(titlestring);

% Now compute mean, 95% confidence intervals and q1-q3 quartiles for the Milstein approximation
figure
lbxItoMilstein = perctile(xItoMilstein',2.5); % Lower Bound of the 95% confidence area obtained by taking, at each time, the 2.5% percentile of the simulated trajectories
waitbar(10/15);
ubxItoMilstein = perctile(xItoMilstein',97.5); % Upper Bound of the 95% confidence area obtained by taking, at each time, the 97.5% percentile of the simulated trajectories
waitbar(11/15);
q1xItoMilstein = perctile(xItoMilstein',25); % first quartile of the simulated trajectories
waitbar(12/15);
q3xItoMilstein = perctile(xItoMilstein',75); % third quartile of the simulated trajectories
waitbar(13/15);
meanxItoMilstein = mean(xItoMilstein,2);
waitbar(14/15);
%                 the empirical 95% CI,                                     first and third quartile                         the process empirical mean
plot(OWNTIME,lbxItoMilstein,'k--',OWNTIME,ubxItoMilstein,'k--',OWNTIME, q1xItoMilstein,'k:',OWNTIME,q3xItoMilstein,'k:',   OWNTIME,meanxItoMilstein,'g-') 
waitbar(15/15);
xlabel('t','Fontsize',13,'Rotation',0);
ylabel('X_t','Fontsize',13,'Rotation',0);
titlestring = sprintf('Ito SDE: mean, 95 percent CI, q1-q3 quartiles of the Milstein approximation over %d trajectories',NUMSIM);
title(titlestring);
close(handle); % close the handle defined with the waitbar command

% Plots the histogram of the trajectories at the end-time (Euler-Maruyama approximation)
figure
hist(xItoEuler(end,:),20);
xlabel('X_T','Fontsize',13,'Rotation',0);
titlestring = sprintf('Ito SDE: Histogram of X_t at end-time T=%d with Euler-Maruyama approximation',OWNTIME(end));
title(titlestring,'Fontsize',12);
% Plots the histogram of the trajectories at the end-time (Milstein approximation)
figure
hist(xItoMilstein(end,:),20);
xlabel('X_T','Fontsize',13,'Rotation',0);
titlestring = sprintf('Ito SDE: Histogram of X_t at end-time T=%d with Milstein approximation',OWNTIME(end));
title(titlestring,'Fontsize',12);


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                      STRATONOVICH SDE COMPUTATIONS
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SDETYPE = 'Strat';  % specifies the SDE type (Ito or Strat (Stratonovich))

xStratMilstein = zeros(N,NUMSIM*NUMDEPVARS);  
errorT_StratMilstein = zeros(NUMSIM,NUMDEPVARS);
lbxStratMilstein = zeros(NUMSIM,NUMDEPVARS);
ubxStratMilstein = zeros(NUMSIM,NUMDEPVARS);
q1xStratMilstein = zeros(NUMSIM,NUMDEPVARS);
q3xStratMilstein = zeros(NUMSIM,NUMDEPVARS);
meanxStratMilstein = zeros(NUMSIM,NUMDEPVARS);

 
%::: computing trajectories with Milstein scheme::::::::::::
xStratMilstein = SDE_milstein_demo(bigtheta);            % the Milstein solution of the Stratonovich SDE, COMPUTED ON THE SAME BROWNIAN PATHS OF THE TRUE SOLUTION
errorT_StratMilstein = abs(xStratMilstein(end,:)-xTrue(end,:));       % the absolute error with Milstein (Stratonovich) solution at time T 


% plot graphs
handle = waitbar(0,'Stratonovich SDE: computing statistics and plotting simulations...');
figure
plot(OWNTIME,xStratMilstein,'k--'), hold on
waitbar(1/8);
plot(OWNTIME,xTrue,'m-'), hold off
waitbar(2/8);
xlabel('t','Fontsize',13,'Rotation',0);
ylabel('X_t','Fontsize',13,'Rotation',0);
titlestring = sprintf('Stratonovich SDE: Milstein vs analytic solution over %d trajectories',NUMSIM);
title(titlestring);

% Now compute mean, 95% confidence intervals and q1-q3 quartiles for the Milstein approximation
figure
lbxStratMilstein = perctile(xStratMilstein',2.5); % Lower Bound of the 95% confidence area obtained by taking, at each time, the 2.5% percentile of the simulated trajectories
waitbar(3/8);
ubxStratMilstein = perctile(xStratMilstein',97.5); % Upper Bound of the 95% confidence area obtained by taking, at each time, the 97.5% percentile of the simulated trajectories
waitbar(4/8);
q1xStratMilstein = perctile(xStratMilstein',25); % the first quartile of the simulated trajectories
waitbar(5/8);
q3xStratMilstein = perctile(xStratMilstein',75); % the third quartile of the simulated trajectories
waitbar(6/8);
meanxStratMilstein = mean(xStratMilstein,2);
waitbar(7/8);
%                 the empirical 95% CI,                                             first and third quartile                      the process empirical mean
plot(OWNTIME,lbxStratMilstein,'k--',OWNTIME,ubxStratMilstein,'k--',OWNTIME,q1xStratMilstein,'k:',OWNTIME,q3xStratMilstein,'k:',   OWNTIME,meanxStratMilstein,'g-') 
waitbar(8/8);
xlabel('t','Fontsize',13,'Rotation',0);
ylabel('X_t','Fontsize',13,'Rotation',0);
titlestring = sprintf('Stratonovich SDE: mean, 95 percent CI, q1-q3 quartiles of the Milstein approximation over %d trajectories',NUMSIM);
title(titlestring);
close(handle); % close the handle defined with the waitbar command

% Plots the histogram of the trajectories at the end-time (Milstein approximation)
figure
hist(xStratMilstein(end,:),20);
xlabel('X_T','Fontsize',13,'Rotation',0);
titlestring = sprintf('Stratonovich SDE: Histogram of X_t at end-time T=%d with Milstein approximation',OWNTIME(end));
title(titlestring,'Fontsize',12);


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                             STATISTICS
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fprintf('\n\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n                                                        ITO SDE WITH EULER-MARUYAMA APPROXIMATION: STATISTICS');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');

avgerrorT_ItoEuler = sum(errorT_ItoEuler)/NUMSIM;             % the average absolute error at time T along the NUMSIM simulations with Euler-Maruyama scheme (Ito SDE)
fprintf('\n\nIto SDE: Average (absolute) error at time %d with Euler-Maruyama method: %d',OWNTIME(end),avgerrorT_ItoEuler);
SDE_stats(bigtheta,xItoEuler,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,'EM',0);

fprintf('\n\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n                                                        ITO SDE WITH MILSTEIN APPROXIMATION: STATISTICS');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');

avgerrorT_ItoMilstein = sum(errorT_ItoMilstein)/NUMSIM;       % the average absolute error at time T along the NUMSIM simulations with Milstein scheme (Ito SDE)
fprintf('\n\nIto SDE: Average (absolute) error at time %d with Milstein method: %d',OWNTIME(end),avgerrorT_ItoMilstein);
SDE_stats(bigtheta,xItoMilstein,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,'MIL',0);

fprintf('\n\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n                                                        STRATONOVICH SDE WITH MILSTEIN APPROXIMATION: STATISTICS');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');
fprintf('\n----------------------------------------------------------------------------------------------------------------------------------------------------------------');

avgerrorT_StratMilstein = sum(errorT_StratMilstein)/NUMSIM;       % the average absolute error at time T along the NUMSIM simulations with Milstein scheme (Stratonovich SDE)
fprintf('\n\nStratonovich SDE: Average (absolute) error at time %d with Milstein method: %d\n\n',OWNTIME(end),avgerrorT_StratMilstein);
SDE_stats(bigtheta,xStratMilstein,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,'MIL',0);