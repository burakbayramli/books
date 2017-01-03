function smoother(Q,R,b)
%SMOOTHER Scalar steady model.
%   	    Forward filtering and smoothing of an observation series b.
%	       System noise covariance Q, observation noise covariance R.

% The included observation series b represents receiver
% clock offsets (ns) from a steered clock.

%Kai Borre 03-27-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global b
e = exist('smoot.eps');
if e ~= 0
   delete smoot.eps
end
e = exist('smootvar.eps');
if e ~= 0
   delete smootvar.eps
end

%b = kalclock('ptb.96o','pta.nav',0);  % Creation of another input set
%b = b*10^9;		 % Conversion of second to nanosecond
if nargin == 2
   b = [...
         -406.05 -331.08 -266.85	 -210.83  -170.77 ...
         -134.14  -96.63  -65.40	  -40.09   -23.08 ...
          -17.42  -17.54  157.41	  331.35   493.32 ...
          642.38  752.80  848.02	  925.23   974.17 ...
          980.48  972.66  929.62	  854.92   734.40 ...
          626.17  541.18  475.70	  421.21   374.44 ...
          322.89  254.76  226.38	  190.00   157.51 ...
          139.05  106.41   75.90	   48.49    17.22 ...
          -19.32  -48.17  -78.52	 -113.18  -145.47 ...
         -170.82 -190.06 -207.77	 -218.61  -226.48 ...
         -237.92 -233.96 -223.53	 -210.70  -196.02 ...
         -186.03 -224.64 -246.42	 -238.71  -193.83 ...
         -172.58 -161.17 -152.88	 -153.72  -156.46 ...
         -164.55 -174.27 -173.64	 -158.99  -152.54 ...
         -145.98 -135.08 -120.72	 -119.30  -112.49 ...
          -88.95  -64.95  -49.72	  -25.61    -0.25];
end

N = size(b,2);
F = 1;
A = 1;
% Initial conditions
x_minus = b(1);
P_minus = 1000;
% Accumulating arrays
x_minus_forw = [];
P_minus_forw = [];
x_plus_forw = [];
P_plus_forw = [];

% Forward Kalman filtering
for i = 0:N-1
   [x_plus, P_plus, K, innovation_var] = ...
                       k_updatx(x_minus, P_minus, A, b(i+1), R, Q);
   x_minus = x_plus;
   P_minus = F*P_plus*F' + Q;
   % Storing
   x_minus_forw = [x_minus_forw x_minus];
   P_minus_forw = [P_minus_forw P_minus];
   x_plus_forw = [x_plus_forw x_plus];
   P_plus_forw = [P_plus_forw P_plus];
   %	  fprintf(' %4g %8.4f %8.4f %8.4f %8.4f %8.4f\n',...
   %		      i, x_plus, P_plus, x_minus, P_minus, b(i+1));
end

% Fixed interval smoothing
x_smoo = zeros(1,N);
P_smoo = zeros(1,N);
% Initial condition  at N for smoothing
P_smoo(N) = P_plus_forw(N);
x_smoo(N) = x_plus_forw(1,N);

% We start the smoothing at k = N-1
for k = N-1:-1:1
   B = P_plus_forw(1,k)*F/P_minus_forw(1,k+1);
   x_smoo(k) = x_plus_forw(k) + B*(x_smoo(k+1) - F*x_minus_forw(k));
   P_smoo(1,k) = P_plus_forw(1,k) + B*(P_smoo(1,k+1) ...
                                          - P_minus_forw(1,k+1))*B';
%   fprintf(' %4g %8.4f %8.4f  %8.4f\n', ...
%                                    k, B, x_smoo(1,k), P_smoo(1,k));
end
t = 0:N-1;

figure;
pl1 = plot(t,b(1:N),'mo', t, x_plus_forw(1:N),'g:', t, x_smoo,'b-');
ylabel('Receiver clock offset [ns]','Fontsize',12)
set(gca,'Fontsize',12);
legend('Observation','Forward state update  ', 'Smoothed estimate  ');
set(pl1,'MarkerSize',6);
set(gca,'Fontsize',12);
legend('Observation','Forward state update  ', 'Smoothed estimate  ');

pause
print smoot -deps
figure;
pl2 = plot(t, P_plus_forw, 'g:', t, P_smoo, 'b-');
ylabel('Error variance of updates','Fontsize',12)
set(gca,'Fontsize',12);
legend('Variance of forward filter ', 'Variance of smoother ');
disp('                           To move the legend, use the mouse.')
disp('                           Anyway press ENTER to continue.')
pause
print smootvar -deps
%%%%%%%% end smoother.m  %%%%%%%%%%%%%%%%%%
