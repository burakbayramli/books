function Y = box(X,flag)
% Box schem for NEWTON method
% flag = 1: Function
% flag = 2: Gradient
% G = Name of problem (globale variable)

global d n G
X = reshape(X,d,n+1); H = 1/n;
switch flag
case 1
   B1 = zeros(d*n,1);
   for j = 1:n
      ARG = (X(:,j) + X(:,j+1))/2;
      B1((j-1)*d+1:j*d) = X(:,j+1) - X(:,j) - H*feval(G,ARG,1);
   end
   XX = [X(:,1);X(:,n+1)];
   B2 = feval(G,XX,3);
   Y  = [B1;B2];
case 2   
   M = sparse(d*(n+1),d*(n+1)); index = 1:d;
   for j = 1:n
      ARG = (X(:,j)+X(:,j+1))/2;
      A   = 0.5*H*feval(G,ARG,2);
      M(index,index) = -(eye(d) + A);
      M(index,index+d) = eye(d) - A;
      index = index + d;
   end
   XX = [X(:,1);X(:,n+1)];
   MM = feval(G,XX,4);
   M(index,[1:d]) = MM(1:d,1:d);
   M(index,index) = MM(d+1:2*d,1:d);
   Y = M;
end
