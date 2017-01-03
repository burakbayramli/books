function x2 = elimnor(A1,A2,A3,b1,b2)
% ELIMNOR Eliminates the unknowns x1
%	       from the normal equations
%	       [A1  A2] |x1| = |b1|
%	       [A2' A3] |x2|   |b2|
%	       and solves for x2

%Kai Borre 07-28-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

if nargin == 0
   A1 = [4];
   A2 = [8];
   A3 = [26];
   b1 = [36];
   b2 = [112];
end;
x2 = inv(A3-A2'*inv(A1)*A2)*(b2-A2'*inv(A1)*b1);
%%%%%%%%%%%%%% end elimnor.m  %%%%%%%%%%%%%%%%%%%%
