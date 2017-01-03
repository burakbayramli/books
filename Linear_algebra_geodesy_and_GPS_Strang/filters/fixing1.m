%FIXING1 Filter version of Examples 12.1, 12.2, and 12.3
%	      Shows the impact on introducing a constraint with
%	      zero variance for the observation

%Kai Borre 07-01-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 06-10-97  $

A = [-1 0 0 1 0;-1 0 0 0 1;0 0 -1 1 0;0 -1 0 0 1;0 0 0 1 -1];
b = [1.978;0.732;0.988;0.420;1.258];
Cov = eye(5);
x = zeros(5,1);
P = 1.e6*eye(5);

% Regular update
for i = 1:5
   [x,P] = k_update(x,P,A(i,:),b(i),Cov(i,i))
   pause
end

% Update with constraint with variance one
A_aug = [1 1 1 1 1];
b_aug = 100;
Cov_aug = 1;
[x,P] = k_update(x,P,A_aug,b_aug,Cov_aug)
Sigma = (norm(b-A*x))^2*P
pause

% Update with constraint with variance zero
Cov_aug = 0;
[x,P] = k_update(x,P,A_aug,b_aug,Cov_aug)
Sigma_plus = (norm(b-A*x))^2*P
%%%%%%%%%%%%%%% end fixing1.m  %%%%%%%%%%%%%%%%%%%%%%
