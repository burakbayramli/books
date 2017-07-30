function heat1dfe
%
% program for solving a 1D heat conduction problem with a finite element
% method and the Jocobi, Gauss-Seidel, and conjugate gradient iterative 
% solvers
%
%     d^2 T/dx^2 + f = 0, for 0<=x<=1 
%  and T = bct0 at x=0, and dT/dx=bct1 at x=1
%
% based on code by Shijie Zhong, modified for Matlab by Thorsten Becker
%
% the Hughes notation is mostly follow here, equation numbers refer to
% the 2000 edition
%
% FILL IN THE BLANKS AT THE ???
%
%
nnperel = 2;				% number of nodes per element

solver = 1;				% 0: Matlab backslash 
                        % direct solver
                        % 1: LU decomposition
                        % iterative solvers:
                        % 2: Jacobi 3: Gauss Seidel
                        % 4: conjugate gradient

tol=1e-5;				% relative solution tolerance

nel = ???;				% number of elements

bct0 = 0;bc_type(1)=0;			    % temperature boundary condition on left
bct1 = 0.1;bc_type(2)=1;			% flux boundary condition on right

L = 1;			              % extent of region

max_iter = 15000;			% maximum number of iterations
%
% derived quantities
%
dx = L/nel;             % element size
node = nel + 1;		    % number of nodes
neq = node;				% number of degrees of freedom

x=(0:node-1)*dx;			% global coordinates of nodes

% set up node assignment structures (those are flipped from Hughes)
%
lm=zeros(nel,nnperel);			% location matrix
ien=zeros(nel,nnperel);			% global node number assignment array
%
% the lm(e,a) matrix will tell which equation in the global system
% belongs to element e and local node number a
%
lm(:,1)=1:nel;
lm(:,2)=lm(:,1) + 1;
%
% set up ien(e,a) array which maps the local node a of element e to
% the global node number
%
ien(:,1)=1:nel;
ien(:,2)=ien(:,1)+1;


%
%  initialize global stiffness matrix and load vector  
%

% global arrays
load     = zeros(neq,1);		% load vector
stiffness = sparse(neq,neq);		% stiffness matrix (only has bandwidth three in this case)

% local
stfm = zeros(nnperel,nnperel);		% local stiffness at element level
xe   = zeros(nnperel,1);			% local node coordinates
loade= zeros(nnperel,1);		    % local load vector

if(nnperel ~= 2)
  error('some computations only defined for two nodes per element')
end

%
% assemble stiffness matrix
%
for e=1:nel				% loop until element before last
  %
  % get global coordinates from global node numbers of each element
  xe(1) = x(ien(e,1));
  xe(2) = x(ien(e,2));
  %
  % size of element 
  %
  dx=xe(2)-xe(1);
  %
  % stiffness matrix at elemental level (1.15.3)
  %
  stfm(1,1) = ???;
  stfm(1,2) = ???;
  stfm(2,1) = ???;
  stfm(2,2) = ???;
  
  % sort into global matrix
  i = lm(e,1);j= i;
  stiffness(i,j) = stiffness(i,j) + stfm(1,1);  
  j = lm(e,2);
  stiffness(i,j) = stiffness(i,j) + stfm(1,2);
  stiffness(j,i) = stiffness(j,i) + stfm(2,1);
  if(e<nel)
      i = lm(e+1,1);
      stiffness(i,j) = stiffness(i,j) + stfm(2,2);
  end

  %
  % elemental load vector, integral over N_a sourcef dx 1.15.6,
  % assuming that sourcef only varies linearly throughout the element
  %
  fac = dx/6;
  loade(1) = (2.0*sourcef(xe(1),L)+    sourcef(xe(2),L))*fac;
  loade(2) = (    sourcef(xe(1),L)+2.0*sourcef(xe(2),L))*fac;

  i = lm(e,1);load(i)=load(i)+loade(1);
  i = lm(e,2);load(i)=load(i)+loade(2);
end
stiffness(neq,neq) = stfm(1,1);% from last element 


%
% BCs
%
i = lm(1,1); % left
if(bc_type(1) == 1)			% flux
    load(i) = load(i) + bct0;
else				% fixed value
    load(i) = bct0 * stiffness(1,1);
end
i = lm(nel,2); % right
if(bc_type(2) == 1)			% flux
    load(i) = load(i) + bct1;
else				% temp
    load(i) = bct1 * stiffness(nel,nel);
end

if(bc_type(1)==0)
    stiffness(1,2)=0;
end
if(bc_type(2)==0)
    i=lm(e,2);
    stiffness(i,i-1)=0;
end


% solve for nodal values 
if(solver == 0)
  sol = stiffness\load;
  iter = 0;  res = 0;
elseif (solver == 1) % LU direct solver
    [l, u ] = lu(stiffness);
    sol = inv(u)*(inv(l)*load);
    iter=0;res=0;
elseif (solver == 2)
  [ sol iter res ] = jacobi(stiffness,load,zeros(neq,1),tol,max_iter);
elseif (solver == 3)
  [ sol iter res ] = gauss_seidel(stiffness,load,rand(neq,1),tol,max_iter);
elseif (solver == 4) % conjugate gradient solver
  sol = cgs(stiffness,load,tol,max_iter);
  iter = 0; res=0;
else
    error('solver number undefined')
end
if(iter == max_iter)
  error('maximum number of iterations reached')
end
if(solver == 2  || solver == 3)
    [ 'residual ' num2str(res) ' number of iterations ' num2str(iter) ]
end
%
% interpolated solution based on elements
%
n=1;
for i=1:nel
    a = ien(i,:); % two global nodes in element 
    for z=-1:.25:1 % element local coordinates
        xi(n) = shape(z)* ??? ;
        ui(n) = shape(z)* ???;
        n=n+1;
    end
end

%
% analytical solution for fixed temp on left and fixed flux on right
% and linear increase in heat production
%
ua = ???
plot(???)
legend('solution on nodes', 'solution within elements','analytical')

% print results
filename=sprintf('sol.%i.solution',solver);
fp = fopen(filename,'w');
fprintf(fp,'%.4e %.4e\n',x,sol);
fclose(fp);

end

%
% heat sources as a function of x
% 
function src = sourcef(x, L)
% linear function of x over domain
src = x/L;
end


%
% shape function in element local coordinates (-1....1)
% for all nodes
function res = shape(zeta)
res(1) = 0.5*(1-zeta);			% left node
res(2) = 0.5*(1+zeta);
end
