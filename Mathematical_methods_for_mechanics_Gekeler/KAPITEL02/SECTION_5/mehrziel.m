function Y = mehrziel(X,flag,G,Parmeter2,Parmeter3)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Multiple shooting for NEWTON method
% flag = 1: Function
% flag = 2: Gradient
n   = Parmeter2(1); % Dimension of problem
m   = Parmeter2(2); % Number of time intervals
MZP = Parmeter2(2+1:end); %m+1 Shooting points
X   = reshape(X,n,m+1);
AG  = zeros(n*m,n);
BG  = zeros(n*m,1);
options = odeset('reltol',1.0E-3,'abstol',1.0E-5);
for j = 1:m
   ANF     = [X(:,j),eye(n)];
   ANF     = ANF(:);
   Flag    = 5;                    %coupled system
   [TA,YA] = ode23(G,[MZP(j), MZP(j+1)],ANF,options,Flag,Parmeter3);
   TL = length(TA); YA = reshape(YA(TL,:),n,n+1);
   BG((j-1)*n+1:j*n)   = YA(:,1) - X(:,j+1);
   AG((j-1)*n+1:j*n,:) = YA(:,2:n+1);
end
if flag == 1
   X0_XT = [X(:,1);X(:,m+1)];
   B2    = feval(G,0,X0_XT,3,Parmeter3);
   Y     = [BG;B2];
end
if flag == 2
   M     = sparse(n*(m+1),n*(m+1));
   index = 1:n;
   for j = 1:m
      M(index,index)   =  AG(index,:);
      M(index,index+n) = - eye(n);
      index            = index + n;
   end
   X0_XT          = [X(:,1);X(:,m+1)];
   MM             = feval(G,0,X0_XT,4,Parmeter3);
   M(index,[1:n]) = MM(1:n,1:n);
   M(index,index) = MM(1:n,n+1:2*n);
   Y = M;
   ESTM = condest(M); % estimation of condition
end
