%Script for Example 11.11
%WC    Filter implementation of impact of changing weights  

%Kai Borre 06-27-97
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1997/10/15  $

big = 1.e8;
A = [1 1; 1 2; -1 1];
b = [2; 1; 0];
Cov = diag([1 1/2 1]); 
%Cov(3,3) = big; % delta C_2 = -1
%Cov(3,3) = 0;  % delta C_2 = \infty
x = zeros(2,1);
P = big*eye(2);

for i = 1:3
   [x,P] = k_update(x,P,A(i,:),b(i),Cov(i,i))
   pause
end   

% the same in Bayes' version
x = zeros(2,1);
P = big*eye(2);
for i = 1:3
   [x,P] = b_row(x,P,A(i,:),b(i),Cov(i,i))
   pause
end   
%%%%%%%%%%%%%%% end wc.m  %%%%%%%%%%%%%%%%%%%%%%   