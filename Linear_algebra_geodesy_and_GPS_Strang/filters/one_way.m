%ONE_WAY  Evaluation of one-way data.
%  	    Observations from Z12 receivers taken at master
%	       site -810 and rover site -005 on day 03-17-94

%Kai Borre 03-26-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global x
clear all
close all

c0 = 299792458;
f1 = 154*10.23e6;
f2 = 120*10.23e6;
lambda1 = c0/f1;
lambda2 = c0/f2;
beta = (f1/f2)^2;
Big = 10^10;

% Automatic input facility
receiver = input('Select master or rover (m, r): ','s');
if receiver ~= ['m' 'r']
   break
end
sv = input('Select Number of SV (2, 9, 16, 23, 26, 27): ');
if sv ~= [2 9 16 23 26 27]
   break
end
datasv = ['one_' receiver int2str(sv)];
filename = [datasv '.dat'];
fid = fopen(filename);
data = fread(fid,inf,'double');
r = size(data,1);
B = reshape(data,r/5,5);

% Due to cold start we drop the first five epochs
range = B(5:90,1:4);
elevation = B(5:90,5);

% Repair of clock reset; affects only pseudoranges
spikes = diff(range(:,1));
i = find(abs(spikes) > 280000);
for j = 1:size(i)
   if spikes(i) < 0
      corr = 299792.458;
   else
      corr = -299792.458;
   end
   for k = i(j)+1:size(range,1)
      range(k,1) = range(k,1)+corr;
      range(k,3) = range(k,3)+corr;
   end
end

r = size(range,1);
P_minus = eye(4)*Big;
% Q = diag([Big Big 0 0]);  system covariance
% This special choice of Q implies that invP_minus can
% be updated the way it is coded below.
% Our implementation avoids the numerical problems inherent
% in the corresponding Kalman filter version.

% Bayes sequential filter, cf.
% Euler & Goad (1991) On Optimal Filtering of GPS Dual Frequency
%			   Observations Without Using Orbit Information
%			   Bulletin Geodesique, 65:130--143.
A = [1	   1	         0 	      0;
     1	  -1	   lambda1	      0;
     1	  beta	      0 	      0;
     1	 -beta	      0 	lambda2];
x_plus = inv(A)*range(1,:)';  % Init. of filter using first obs.
x = [];
P = [];
% P = P + Q; Kalman version uses this.
% Bayes version uses inverse of information (or weight) matrix: invR
%  weight = [1/.3^2 1/0.003^2 1/.3^2 1/0.003^2];  % variance of obsv.
%  invR = diag(weight);

for k = 1:r
   H = inv(P_minus(3:4,3:4));
   invP_minus = [zeros(2,4); zeros(2,2) H];
   x_minus = x_plus;
   % We make the variance for pseudoranges elevation dependent
   sigma = 0.08 + 4.5*exp(-elevation(k)/10);
   weight = [1/sigma^2 1/0.003^2 1/sigma^2 1/0.003^2];
   invR = diag(weight);
   ATR_inv = A'*invR;
   P_plus = inv(invP_minus + ATR_inv*A);
   K = P_plus*ATR_inv;
   x_plus = x_minus+K*(range(k,:)' - A*x_minus);
   P_minus = P_plus;
   x = [x x_plus];     % x = [rho*; I; N1; N2]
   P = [P P_plus];
end
fprintf('\nEstimated ambiguity for N1-N2: %12.1f', x(3,r)-x(4,r))
fprintf('\nEstimated ambiguity for N1:    %12.1f\n', x(3,r))
% Compute eigenvalues for N1-N2 and N1+N2 combinations
Cov = P_plus(3:4,3:4);
D = [1 -1;1 1];
DCov = D*Cov*D' %;
[evec, ev] = eig(DCov);
fprintf('\nEigenvalues of covariance matrix for wide and narrow lanes:')
fprintf('\n                          %3.5f  %3.5f\n', ev(2,2), ev(1,1))
% Uncomment the following line if you want to plot the support func.
% support(DCov)

figure(1);
plot(x(1,:)-mean(x(1,:)))   % rho*
title('rho*')
figure(2);
plot(x(2,:)-mean(x(2,:)))   % I
title('Ionospheric delay')
figure(3);
plot(x(3,:)-x(4,:)-(mean(x(3,:))-mean(x(4,:))))  % N1-N2
title('N1-N2')
figure(4);
plot(x(3,:)-mean(x(3,:)))   % N1
title('N1')
figure(5);
plot(sqrt(P(4,4:4:4*r)));   % standard dev. of update of N2
title('Standard deviation of update of N2')
%%%%%%%%%%%%%% one_way.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
