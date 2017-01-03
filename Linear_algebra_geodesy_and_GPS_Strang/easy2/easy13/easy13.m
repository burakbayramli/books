
%EASY13 Script for computing parity vector and HPL

%Kai Borre 01-07-2008
%Copyright (c) by Kai Borre
% Revison 1.2 is due to a kind remark by Dr. David Y. Hsu

%$Revision: 1.2 $  $Date: 2009/09/05  $

b = [    -1.05;
          0.39;
         -0.40;
         -1.33;
          2.76;
          1.60;
         -1.98];

A = [    -0.55         -0.33         -0.76          1.00;
         -0.39          0.53         -0.76          1.00;
         -0.68          0.72          0.16          1.00;
         -0.88          0.23         -0.41          1.00;
         -0.42         -0.78         -0.45          1.00;
          0.21          0.71         -0.66          1.00;
          0.52         -0.53         -0.66          1.00];
   
% First version      
[U,Sigma,V] = svd(A);
p1 = U(:,5:end)'*b;
sd2 = sqrt(p1'*p1);
% Second version
[Q,R] = qr(A);
p2 = Q(:,5:end)'*b

dtr =pi/180;

% You may change the script to a function with phi and lambda as parameters 
phi = 57;
lambda = 10;

cl = cos(lambda*dtr); sl = sin(lambda*dtr);
cb = cos(phi*dtr); sb = sin(phi*dtr);

F = [ -sl    cl  0;
    -sb*cl -sb*sl cb;
    cb*cl  cb*sl sb];

m = size(A,1);
S = eye(m)-A*inv(A'*A)*A';
sd1 = sqrt((b'*b)/(m-4))
A0 = A(:,1:3);
M = F*inv(A0'*A0)*A0';

alpha = zeros(m,1);
for i = 1:m
   alpha(i,1) = sqrt((M(1,i)^2+M(2,i)^2)/diag(S(i,i)));
end 
alpha_max = max(alpha)
sigma = norm(p2)/sqrt(m-4)
sigma1 = sd2/sqrt(m-4)
HPL = alpha_max*sigma

%%%%%%%%%%%%%%%%%%%%%%%%%%% easy13.m  %%%%%%%%%%%