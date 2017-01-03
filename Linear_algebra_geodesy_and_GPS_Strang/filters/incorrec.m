%INCORREC  Random walk incorrectly modeled as a random constant.

%Kai Borre, 04-13-97
%Copyright (c) 1997 by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

e = exist('incorrec.eps');
if e ~= 0
   delete incorrec.eps
end

% Random walk process simulated using Gaussian random numbers
% with zero mean and unit variance ~ N(0,1)
tmax = 100;
t = 0:tmax;
rwp = randn(size(t));

%We assume the following lines generate a one-dimensional random walk
rw(1) = rwp(1);
for i = 1:tmax
   rw(i+1) = rw(i) + rwp(i+1);
end

% Generation of true model
F = 1;
A = 1;
Q = 1;
R = 0.1;
% Initial conditions
x_minus = 0;
P_minus = 1;
% Artificial way of creating observations
b = rw + randn(size(t));
% Kalman filtering
for i = 0:tmax
   [x_plus, P_plus, K, innovation_var] = ...
               k_updatx(x_minus, P_minus, A, b(i+1), R, Q);
   % fprintf(' %4g %4g \n',i,K);
   x_minus = x_plus;
   P_minus = P_plus;
   % Storing
   x_plus_corr = [x_plus_corr x_plus];
   % fprintf(' %4g %8.4f %8.4f %8.4f %8.4f %8.4f\n',...
                  % i, x_plus, P_plus, x_minus, P_minus, b(i+1));
end

% Generation of incorrect model
Q = 0;
% Initial conditions
x_minus = 0;
P_minus = 1;
% Kalman filtering
for i = 0:tmax
   [x_plus, P_plus, K, innovation_var] = ...
                   k_updatx(x_minus, P_minus, A, b(i+1), R, Q);
   %  fprintf(' %4g %4g \n',i,K);
   x_minus = x_plus;
   P_minus = P_plus;
   % Storing
   x_plus_inc = [x_plus_inc x_plus];
   % fprintf(' %4g %8.4f %8.4f %8.4f %8.4f %8.4f\n',...
                      % i, x_plus, P_plus, x_minus, P_minus, b(i+1));
end

figure;
pl1 = plot(t,b,'yo',t,rw,'r-',t,x_plus_corr,'b:',t,x_plus_inc,'c--');
set(pl1,'Markersize',4);
title('Simulated random walk process','Fontsize',16)
ylabel('State variable','Fontsize',16)
legend('Observations','Random walk process', ...
                'Filter with correct model',...
                'Filter with incorrect model');
set(gca,'Fontsize',16);
legend('Observations','Random walk process',...
                 'Filter for correct model',...
                 'Filter for incorrect model');
pause
print incorrec -deps
%%%%%%%% end incorrec.m  %%%%%%%%%%%%%%%%%%
