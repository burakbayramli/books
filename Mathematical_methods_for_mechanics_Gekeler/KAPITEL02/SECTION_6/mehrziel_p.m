function [Y,V] = mehrziel_p(X,flag,GG,Parmeter2,Parmeter3)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Multiple shooting for Newton method
% Problem with estimated period
% flag = 1: Function
% flag = 2: Gradient
n   = Parmeter2(1); % Dimension of problem
m   = Parmeter2(2); % Number of subintervals
%bsp = Parmeter2(3); % Beispiel-Nr.
TT  = linspace(0,1,m+1); %Equistiant shooting points
LE  = length(X);
Periode   = X(LE);
Parmeter4 = [Periode,Parmeter3];
X   = X(1:LE-1);
X   = reshape(X,n,m+1);
AG  = zeros(n*m,n); BG  = zeros(n*m,1); BP  = zeros(n*m,1);
AW  = Parmeter2(3:n*(m+1)+2);
AW  = reshape(AW,n,m+1);
options = odeset('reltol',1.0E-5,'abstol',1.0E-8);
% -- Values for system and gradients --------
for j = 1:m
   ANF     = [X(:,j),eye(n),AW(:,j)];
   ANF     = ANF(:);
   Flag    = 2;                    % coupled system
   [TA,YA] = ode23(GG,[TT(j),TT(j+1)],ANF,options,Flag,Parmeter4);
   TL                  = length(TA);
   YA                  = reshape(YA(TL,:),n,n+2);
   BG((j-1)*n+1:j*n)   = YA(:,1) - X(:,j+1);%for Function
   AG((j-1)*n+1:j*n,:) = YA(:,2:n+1);       %for Gradient
   VALT                = Parmeter2(j*n+3:(j+1)*n+2);
   Parmeter2(j*n+3:(j+1)*n+2) = YA(:,n+2);
   BP((j-1)*n+1:j*n)   = YA(:,n+2) - VALT;    % for last column
end
V = Parmeter2(3:n*(m+1)+2);
if flag == 1
   % -- Value of function ----------------------------------
   X0_XT = [X(:,1);X(:,m+1)];
   B2    = feval(GG,[],X0_XT,3,Parmeter4); % boundary condition
   Y     = [BG;B2];
end
if flag == 2
   % -- Gradient ---------------------------------------
   M     = sparse(n*(m+1),n*(m+1)); % large matrix
                                    % without last column
   index = 1:n;
   for j = 1:m % Filling large matrix
      M(index,index)   =  AG(index,:);
      M(index,index+n) = - eye(n);
      index            = index + n;
   end
   % -- last block row with boundary conditions -----------
   X0_XT          = [X(:,1);X(:,m+1)];
   MM             = feval(GG,[],X0_XT,4,Parmeter4);
   M(index,[1:n]) = MM(1:n,1:n);
   M(index,index) = MM(1:n,n+1:2*n);
   % -- last column  --------------------------------
   BP = [BP;zeros(n,1)];
   M  = [M,BP];
   Y = M;
end
