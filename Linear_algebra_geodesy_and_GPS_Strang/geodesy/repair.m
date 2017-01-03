function R = repair(S)
%REPAIR  Repair of indefinite covariance matrix

%Kai Borre 07-22-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1998/03/30  $
%Corrected according to advice by David Hsu

if nargin == 0
   S = [3 3 0;3 -1 0;0 0 -3]; 
end
[phi,lambda] = eig(S);
[n,n] = size(S);
R = zeros(n,n);
for i = 1:n
   if lambda(i,i) > 0
      ss = lambda(i,i)*phi(:,i)*phi(:,i)'; 
      R = R+ss;
   end
end;
%%%%%%%%%%%%%% end repair.m %%%%%%%%%%%%%%%%%
