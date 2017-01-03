function k_dd3(sv)
%K_DD3  Kalman Filter for Estimation of Ambiguities (with I = 0)
%   	  Double differenced code and phase observations
%	     SV is the satellite to be differenced with ref. sat. 26.
%	     The choices are: 2, 9, 16, 23, 27

%Kai Borre 01-25-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1997/12/06  $

%Data in dds26_*.dat are arranged with one epoch a line
%(Data are created by lwii.m)
%All observations are P-observations made by an Ashtech Z-12:
%Pcode_1   phase_1   Pcode_2	 phase_2

%  Some initial computations of constants
c0 = 299792458;	  % velocity of light m/s
f1 = 154*10.23E6;	  % L1 frequency Hz
f2 = 120*10.23E6;	  % L2 frequency Hz
lambda1 = c0/f1;	  %  .19029367	 m
lambda2 = c0/f2;	  %  .244210213  m

% Automatic input facilities
datasv = ['dds26_' int2str(sv)];
filename = [datasv '.dat'];
eval(['load ' filename]);
b = eval(datasv);
[m,n] = size(b);

% Definition of filter matrix
A = [ones(4,1) zeros(4,2)];
A(2,2) = lambda1;
A(4,3) = lambda2;

% Covariance matrix for observations
Sigma_e = diag([0.09 0.000025 0.09 0.000025]);
Sigma_eps = diag([10 0 0]); % chance in x from epoch to epoch

% Covariance matrix for state vector
ef = 5;                   % first epoch
el = m;                   % last epoch
x = zeros(3,el-ef+1);
lss = zeros(3,el-ef+1);   % storage of epoch-wise least squares solutions

% Initial Values
x_minus = A\b(ef,:)';
Sigma_plus = diag([10 10 10]); % variance for x at start

for i = ef:el
   Sigma_minus = Sigma_plus+Sigma_eps;
   K = Sigma_minus*A'*inv(A*Sigma_minus*A'+Sigma_e);
   x_plus = x_minus+K*(b(i,:)'-A*x_minus);
   x(:,i-ef+1) = x_plus;
   Sigma_plus = (eye(3)-K*A)*Sigma_minus;
   x_minus = x_plus;
   lss(:,i-ef+1) = A\b(i,:)';
   K1 = round(x(2,i-ef+1)-x(3,i-ef+1));
   K2 = round(60*b(i,2)'/lambda1-77*b(i,4)'/lambda2);
   trueN2 = round((60*K1-K2)/17);
   trueN1 = round(trueN2+K1);
   fprintf(['\nN1: Filter %10.1f Goad %10.1f'...
	       ' Nw: Filter %10.1f Goad %10.1f'], x_minus(2), ...
         	       trueN1, x_minus(2)-x_minus(3), trueN1-trueN2)
end;
wl = trueN1-trueN2;
clf
t = ef:el;
h1 = figure(1);
hold on
   plot(t,x(2,:)-x(3,:)-wl,'o',t,lss(2,:)-lss(3,:)-wl,'*')
   xlabel('Epochs [epoch interval 20 s]');
   ylabel('N_1-N_2 [cycles]');
   title(['Double difference N_1-N_2 ionosphere free estimates,' ...
          ' Z-12 receivers, March 17, 1994']);
   legend('Filtered ''Goad''-values   ','Batch values');
   zoom
hold off
fprintf('\n')
print k_dd3 -deps
%%%%%%% end k_dd3.m  %%%%%%%%%%%%%%%%%
