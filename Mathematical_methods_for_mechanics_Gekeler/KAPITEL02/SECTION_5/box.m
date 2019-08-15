function Y = box(X,flag,G,Parmeter2,Parmeter3)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Box scheme for NEWTON method
% flag = 1: Funktion
% flag = 2: Gradient
n  = Parmeter2(1); % Dimension of problem
m  = Parmeter2(2); % number of sub-intervals
X  = reshape(X,n,m+1); H = 1/m;
switch flag
case 1
   B1 = zeros(n*m,1);
   for j = 1:m
      TT  = H*((j-1) + 0.5);
      ARG = (X(:,j) + X(:,j+1))/2;
      B1((j-1)*n+1:j*n) = X(:,j+1) - X(:,j) - H*feval(G,TT,ARG,1,Parmeter3);
   end
   X0_XT = [X(:,1);X(:,n+1)];
   B2    = feval(G,TT,X0_XT,3,Parmeter3);
   Y     = [B1;B2];
case 2
   M = sparse(n*(m+1),n*(m+1));
%   M         = zeros(n*(m+1),n*(m+1));
   index = 1:n;
   for j = 1:m
      TT  = H*((j-1) + 0.5);
      ARG = (X(:,j)+X(:,j+1))/2;
      A   = 0.5*H*feval(G,TT,ARG,2,Parmeter3);
      M(index,index)   = - eye(n) - A;
      M(index,index+n) =   eye(n) - A;
      index = index + n;
   end
   X0_XT = [X(:,1);X(:,m+1)];
   MM    = feval(G,TT,X0_XT,4,Parmeter3);
   M(index,[1:n]) = MM(1:n,1:n);
   M(index,index) = MM(1:n,n+1:2*n);
   Y = M;
end
