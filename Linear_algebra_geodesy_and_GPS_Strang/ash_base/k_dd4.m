function  k_dd4(sv)
%K_DD4 Kalman Filter for Estimation of Ambiguities
%    	 Double differenced code and phase observations

%Kai Borre 01-25-96
%Copyright (c) 1997 by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

% Data in ddsv26_*.mat are arranged with one epoch a line
% (created by lwii.m).  Reference sv is #26  and the second sv is #*.
% All observations are made by an Ashtech Z-12 receiver.
% Sequence of obsv. in b:  Pcode_1   phase_1	Pcode_2   phase_2

% Some initial computations of constants 
c0 = 299792458;	  % velocity of light m/s
f1 = 154*10.23E6;	  % L1 frequency Hz
f2 = 120*10.23E6;	  % L2 frequency Hz
lambda1 = c0/f1;	  %  .19029367	 m
lambda2 = c0/f2;	  %  .244210213  m
beta = (f1/f2)^2;	  % 1.646944....

datasv = ['dds26_' int2str(sv)];
filename = [datasv '.dat'];
eval(['load ' filename]);
b = eval(datasv);
[m,n] = size(b);
const(2)  =	 -104540;
const(9)  = -1340027;
const(16) = -3918356;
const(23) =	   11107;
const(27) = -3705777;
wl = const(sv);

% Coefficient matrix
A = [ones(4,2) zeros(4,2)];
A(2,2) = -A(2,2);
A(3,2) = beta;
A(4,2) = -A(3,2);
A(2,3) = lambda1;
A(4,4) = lambda2;

% Covariance matrix for observations
Sigma_e = diag([0.1 0.000025 0.1 0.000025]);

% Covariance matrix for state vector
Sigma_eps = diag([100 1 0 0]);   % change in x from epoch to epoch

ef = 5;  % first epoch
el = m;  % last epoch
x = zeros(4,el-ef+1);
lss = zeros(4,el-ef+1); % store for epoch-wise least squares solutions

% Initial values
Sigma_plus = 10*eye(4,4); % start variance for state vector
x_minus = A\b(ef,:)';

for i = ef:el
   Sigma_minus = Sigma_plus+Sigma_eps;
   K = Sigma_minus*A'*inv(A*Sigma_minus*A'+Sigma_e);
   x_plus = x_minus+K*(b(i,:)'-A*x_minus);
   x(:,i-ef+1) = x_plus;
   Sigma_plus = (eye(4)-K*A)*Sigma_minus;
   x_minus = x_plus;
   fprintf('rho* %6.3f iono %6.3f N1 %10.3f dNw %10.3f \n', ...
	  x(1,i-ef+1), x(2,i-ef+1), x(3,i-ef+1), x(3,i-ef+1)-x(4,i-ef+1)-wl);
   lss(:,i-ef+1) = A\b(i,:)';
end;

t = ef:el;
h1 = figure(1);
hold on
   pl1 = plot(t,x(3,:)-x(4,:)-wl,'o',t,lss(3,:)-lss(4,:)-wl,'*');
   set(pl1,'Markersize',4);
   xlabel('Epochs [epoch interval 20 s]');
   ylabel('{\itN_1-N_2} [cycles]');
   title(['Double difference {\itN_1-N_2} epoch-by-epoch estimates,' ...
                           		 ' Z-12 receivers, March 17, 1994']);
   legend('Filtered Values', 'Epoch-by-Epoch Solution, Given {\itN_w}  ');
   zoom
hold off

h2 = figure(2);
hold on
   pl2 = plot(t,x(2,:),'o',t,lss(2,:),'*');
   set(pl2,'Markersize',4);
   xlabel('Epochs [epoch interval 20 s]');
   ylabel('Double difference ionosphere {\itI} [m]');
   title(['Double difference ionospheric delay estimates' ...
                       		 ' of 4595.12 m vector, March 17, 1994']);
   legend('Filtered Values', 'Epoch-by-Epoch Solution',...
                                        '    Given {\itN_{w}}');
   zoom
hold off
%%%%%%%%% end k_dd4.m %%%%%%%%%%%%%%%%
