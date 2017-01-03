function x2 = elimobs(A1,A2,b,C)
%ELIMOBS Eliminates the unknowns x1
%	      from the observation equations
%	      [A1  A2] |x1| = b
%		            |x2|
%	      and solves the weighted least squares
%	      problem for x2 from the remaining
%	      observation equations

%Kai Borre 07-28-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

if nargin == 0
   A1 = [1;1;1;1];
   A2 = [0;1;3;4];
   b = [0;8;8;20];
   C = eye(4,4);
end
[m,n] = size(b);
P = eye(m,m)-A1*inv(A1'*C*A1)*A1'*C;
A2star = P*A2;
bstar = P*b;
x2 = inv(A2star'*C*A2star)*A2star'*C*bstar;
%%%%%%%%%%%%%%%%%%% end elimobs.m %%%%%%%%%%%%%%%%

