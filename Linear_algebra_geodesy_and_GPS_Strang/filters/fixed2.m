%FIXED2 Solution to Example 10.1.
%       Solution to Example 12.4.
%       Solution to Example 12.7
%       Solution as descibed by equation (12.64).
%       Solution obtained by filtering.

%Kai Borre 07-03-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 06-10-97  $

format short
% General input
A = [-1      0	     1      0      0      0      0      0;
      0.707  0.707  0      0     -0.707 -0.707  0      0
      0.707 -0.707  0      0      0      0     -0.707  0.707;
      0      0      0.924  0.383 -0.924 -0.383  0      0;
      0      0      0      0      0     -1      0      1;
      0      0      0.924 -0.383  0      0     -0.924  0.383];
G = [   1      0       1      0       1   0    1      0;
        0      1       0      1       0   1    0      1;
   -170.71 170.71 -170.71 270.71 -100 100 -241.42 100;
   170.71 170.71  270.71 170.71  100 100  100    241.42]';
obs = [100.01; 100.02; 100.03; 184.785; 141.44; 184.805];
xp =170.71;
yp = 170.71;
x0 = [270.71; 170.71; 100; 100; 100; 241.42];   
b(1,1) = obs(1)-sqrt((x0(1)-xp)^2+(x0(2)-yp)^2);
b(2,1) = obs(2)-sqrt((x0(3)-xp)^2+(x0(4)-yp)^2);
b(3,1) = obs(3)-sqrt((x0(5)-xp)^2+(x0(6)-yp)^2);
b(4,1) = obs(4)-sqrt((x0(1)-x0(3))^2+(x0(2)-x0(4))^2);
b(5,1) = obs(5)-sqrt((x0(3)-x0(5))^2+(x0(4)-x0(6))^2);
b(6,1) = obs(6)-sqrt((x0(5)-x0(1))^2+(x0(6)-x0(2))^2); 
% Solution for Example 10.1
AA = A(1:3,1:2);
bb = b(1:3);
x = AA\bb
X = [xp;yp]+x
rsquared = (norm(bb-AA*x))^2

% Solution for Example 12.4
bx = [.01;.02;.03;.01;.02;.03];
x = pinv(A)*bx
r = bx-A*x
sigma = norm(r)

% Solution for Example 12.7
AA = A(:,1:8); % number of constraints may be changed by the "4" 
x = AA\b
r = b-AA*x
sigma0 = norm(r)
Sigma = sigma0^2*pinv(AA'*AA)

break
Sp = [zeros(4,8); -G(5:8,:)*inv(G(1:4,:)) eye(4)];
xp =Sp*x

A1 = A(:,1:2);
A2 = A(:,3:8);
b0 = x0;
xn = inv(A1'*A1)*A1'*(b-A2*b0);  % (12.64)
xn
% Regular update
for i = 1:6
   [x,P] = k_update(x,P,A(i,:),b(i),Cov(i,i));
   for j = 1:8    
      fprintf('%8.4f',x(j))
   end
   fprintf('\n')
end

% Update with constraint with variance one
b_aug = [0; 0; 0];
Cov_aug = 0;
for i = 1:3
   [x,P] = k_update(x,P,G(:,i)',b_aug(i),Cov_aug);
   for j = 1:8
      fprintf('%8.4f',x(j))
   end
   fprintf('\n')
end

% Fixing the first three coordinates. This secures x(1)= x(2)= x(3)= 0
H = zeros(1,8);
b_H = 0;
Cov_H = 0;
for i = 1:3
   H(i) = 1;
   [x,P] = k_update(x,P,H,b_H,Cov_H);
end   
fprintf('\n')
for j = 1:8
   fprintf('%8.4f',x(j))
end 
fprintf('\n')
Sigma = (norm(b-A*x))^2*P
%%%%%%%%%%%%%%% end fixed2.m  %%%%%%%%%%%%%%%%%%%%%%
