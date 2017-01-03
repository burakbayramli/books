%RTS    Calculation of filtered and smoothed estimates
%	     of covariances. The observations and the state
%	     vector is of no concern in this example.

%	 Numerical examples from
%	    Rauch, H. E., F. Tung, and C. T. Striebel (1965)
%		 Maximum Likelihood Estimates of Linear Dynamic Systems.
%	    American Institute of Aeronautics ans Astronautics Journal
%	    Vol. 3, pp. 1445--1450

%  Covariance of system noise Q
%  Covariance of observation noise R

%Kai Borre 05-30-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

e = exist('rtsvar.eps');
if e ~= 0, delete('rtsvar.eps'), end

% Fixed interval smoothing
N = 25;
P_forward = zeros(3,N+1);
P_smooth = zeros(3,N+1);
Q = zeros(4,4);
R = 1;
F = [1   1    .5   .5   ;
     0   1   1    1     ;
     0   0   1    0     ;
     0	0   0	    .606];
A = [1 0 0 0];
b = 1;

% Initial conditions
x_minus = ones(4,1);

for ex = 1:3
   if ex == 1
      Q(4,4) = .0063;
      P_minus = eye(4);
      P_minus(4,4) = .01;
   end
   if ex == 2
      Q(4,4) = .63*1.e-4;
      P_minus = eye(4);
      P_minus(4,4) = .0001;
   end
   if ex == 3
      P_minus = 100*eye(4);
      P_minus(4,4) = .01;
   end
   % Accumulating arrays
   P_minus_forw = [];
   P_plus_forw = [];
   % Forward Kalman filtering
   for i = 0:N
      P_minus_forw = [P_minus_forw P_minus];
      [x_plus, P_plus] = k_update(x_minus, P_minus, A, b, R);
      if i == 0
         P_forward(ex,1) = 1;
         P_plus(1,1) = 1;
         P_plus_forw = [P_plus_forw P_plus];
      else
         P_forward(ex,i+1) = P_plus(1,1);
         P_plus_forw = [P_plus_forw P_plus];
      end
      P_minus = F*P_plus*F' + Q;
   end
   % Initial condition at N+1 for smoothing
   P_smooth(ex,N+1) = P_forward(ex,N+1);
   P_smoo = P_plus;
   % We start the smoothing at k = N
   for k = N:-1:1
      % We use (3.29) and (3.31) in Rauch et al.
      C = P_plus_forw(:,4*(k-1)+1:4*(k-1)+4)*F'...
                                 *inv(P_minus_forw(:,4*k+1:4*k+4));
      P_smoo = P_plus_forw(:,4*(k-1)+1:4*(k-1)+4) + C*(P_smoo -...
                                   P_minus_forw(:,4*k+1:4*k+4))*C';
      P_smooth(ex,k) = P_smoo(1,1);
   end
end % ex

t = 0:N;
figure;
hold on
plot(t, P_forward(1,:), '-', t, P_forward(2,:),'-.',...
                                           t, P_forward(3,:),'--')
set(gca,'Fontsize',16);
legend('Case 1', 'Case 2', 'Case 3');
plot(t, P_smooth(1,:), '-', t, P_smooth(2,:), '-.',...
                                           t, P_smooth(3,:), '--')
hold off
% text(14,.6,'Filtered estimates {\it\bfx}_{{\itk}|{\itk}}','Fontsize',16)
% text(8,.2,'Smoothed estimates {\it\bfx}_{{\itk}|25}','Fontsize',16)
xlabel('Observation {\itk}','Fontsize',16)
ylabel('Variance of {\itx}_1','Fontsize',16,...
                                       'VerticalAlignment','bottom')
print rtsvar -deps
%%%%%%%% end rts.m  %%%%%%%%%%%%%%%%%%
