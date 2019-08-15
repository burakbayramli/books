function Y = box(X,flag,G,Parmeter2,Parmeter3)
% Box scheme for NEWTON method
% flag = 1: Function
% flag = 2: Gradient
d = Parmeter2(1); % Dimension of problem
n = Parmeter2(2); % Number of time intervals
T = Parmeter2(3); % Operational time

X = reshape(X,d,n+1); H = T/n;
switch flag
case 1
   B1 = zeros(d*n,1);
   for j = 1:n
      ARG = (X(:,j) + X(:,j+1))/2;
      B1((j-1)*d+1:j*d) = X(:,j+1) - X(:,j) - H*feval(G,ARG,1,Parmeter3);
   end
   X0_XT = [X(:,1);X(:,n+1)];
   B2    = feval(G,X0_XT,2,Parmeter3);
   Y     = [B1;B2];
case 2
   M = sparse(d*(n+1),d*(n+1));
   index = 1:d;
   for j = 1:n
      ARG = (X(:,j)+X(:,j+1))/2;
      A = 0.5*H*feval(G,ARG,3,Parmeter3);
      M(index,index)   = - eye(d) - A;
      M(index,index+d) =   eye(d) - A;
      index = index + d;
   end
   X0_XT          = [X(:,1);X(:,n+1)];
   MM             = feval(G,X0_XT,4,Parmeter3);
   M(index,[1:d]) = MM(1:d,1:d);
   M(index,index) = MM(1:d,d+1:2*d);
   Y = M;
end
