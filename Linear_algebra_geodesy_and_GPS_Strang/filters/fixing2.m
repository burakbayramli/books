%FIXING2 Filter version of Examples 12.4 and 12.7.
%	      Shows the impact on introducing constraints 
%	      as observations with zero variance 

%Kai Borre 07-10-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 06-10-97  $

format short
A = [-1      0	     1      0      0      0       0      0;
      0.707  0.707  0      0     -0.707 -0.707   0      0
      0.707 -0.707  0      0      0      0      -0.707  0.707;
      0      0      0.924  0.383 -0.924 -0.383   0      0;
      0      0      0      0      0     -1       0      1;
      0      0      0.924 -0.383  0      0      -0.924  0.383];
b = [0.01; 0.02; 0.03; 0.01; 0.02; 0.03];
Cov = eye(6);
x = zeros(8,1);
P = 1.e6*eye(8);

% Regular update
for i = 1:6
   [x,P] = k_update(x,P,A(i,:),b(i),Cov(i,i));
   for j = 1:8
      fprintf('%8.4f',x(j))
   end
   fprintf('\n')
end
fprintf('\n')

% Update with constraints, that is observations with variance zero
G = [   1      0       1      0       1   0    1      0;
        0      1       0      1       0   1    0      1;
     -170.71 170.71 -170.71 270.71 -100 100 -241.42 100;
      170.71 170.71  270.71 170.71  100 100  100    241.42]';
% Baarda theory  
Sp = [zeros(4,8); -G(5:8,:)*inv(G(1:4,:)) eye(4)];
Cov = (norm(b-A*x))^2*pinv(A'*A);
Sigma_xp = Sp*Cov*Sp';   

% Fixing the first four coordinates to zero values
H = zeros(1,8);
b_fix = 0;
Cov_fix = 0;
for i = 1:4
   H(i) = 1;
   [x,P] = k_update(x,P,H,b_fix,Cov_fix);
   fprintf('\n')
   for j = 1:8
      fprintf('%8.4f',x(j))
   end 
end
fprintf('\n')

% Filter for a similarity transformation
b_fix = 0;
Cov_fix = 0;
for i = 3:4
   [x,P] = k_update(x,P,G(:,i)',b_fix,Cov_fix);
   fprintf('\n')
   for j = 1:8
      fprintf('%8.4f',x(j))
   end 
end
fprintf('\n')   
Sigma = (norm(b-A*x))^2*P;   
%%%%%%%%%%%%%%% end fixing2.m  %%%%%%%%%%%%%%%%%%%%%%
