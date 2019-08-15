function [V,C] = bdf(flag,U);
% Computation of C and C*U with trig. collocation
% flag = 1: backward differences
% flag = 2: backward differences inverse
[p,n] = size(U); V = zeros(p,n); c = n/(4*pi);
C = zeros(n,n);
C(1,1) = 3; C(1,n-1) = 1; C(1,n) = - 4;
C(2,1) = - 4; C(2,2) = 3; C(2,n) = 1;
AUX = [1,-4,3];
for k = 1:n-2
   C(k+2,[k,k+1,k+2]) = AUX;
end
C = C*c;
switch flag
case 1, 
   V(:,1) = (3*U(:,1) - 4*U(:,n) + U(:,n-1))*c;
   V(:,2) = (3*U(:,2) - 4*U(:,1) + U(:,n))*c;
   V(:,3:n) = (3*U(:,3:n) - 4*U(:,2:n-1) + U(:,1:n-2))*c;
case 2, 
   V(:,1:n-2)   = (3*U(:,1:n-2)   - 4*U(:,2:n-1) + U(:,3:n))*c;
   V(:,n-1) = (3*U(:,n-1) - 4*U(:,n) + U(:,1))*c;
   V(:,n)   = (3*U(:,n)   - 4*U(:,1) + U(:,2))*c;
end

