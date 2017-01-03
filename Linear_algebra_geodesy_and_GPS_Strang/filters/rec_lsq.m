function rec_lsq(A,b,Sigma)
%REC_LSQ Recursive Least Squares
%        A is the coefficient matrix, b the observations and 
%        Sigma a vector containing the diagonal entries of
%        the covariance matrix for the problem.
%        For increasing i we include one more observation 

%Kai Borre 09-16-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

if nargin == 0
   A = [1 0;1 1;1 3;1 4];
   b = [0;8;8;20];
   Sigma = diag([1,1,1,1]);
end

invSigma = inv(Sigma(1,1));
% Initial weight
P = A(1,:)'*invSigma*A(1,:);
if rcond(P) == 0
   P = 1.e10*eye(size(A,2));
else 
   P = inv(P);
end 
% Initial solution
x = pinv(A(1,:)'*invSigma*A(1,:))*A(1,:)'*invSigma*b(1);

for i = 1:size(b,1)
   invSigma = inv(Sigma(i,i));
   AtinvS =A(i,:)'*invSigma;
   P = inv(inv(P)+AtinvS*A(i,:)) %;
   K = P*AtinvS;
   x = x+K*(b(i)-A(i,:)*x);
   fprintf('\nSolution:\n');
   for j = 1:size(A,2)
      fprintf('  x(%2g) = %6.3f\n',j,x(j));
   end
end

dof = size(b,1)-size(A,2);
if dof ~= 0
   P = (norm(b-A*x))^2*P/dof;
else
   P = (norm(b-A*x))^2*pinv(A'*Sigma*A);  
end      
fprintf('\nFinal Covariance matrix:\n');
for j = 1:size(A,2)
   for k = 1:size(A,2)
      fprintf('%12.3f',P(j,k));
   end
   fprintf('\n');
end
fprintf('\nTrace of Covariance matrix: %12.3f\n',trace(P));
%%%%%%%%%%%%%%%%% end rec_lsq.m %%%%%%%%%%%%%%%%%%%




