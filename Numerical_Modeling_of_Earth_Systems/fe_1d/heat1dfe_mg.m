function heat1dfe_mg
% program for solving a 1D heat conduction problem with a finite element
% method using multigrid solvers
%
%     d^2 T/dx^2 + f = 0, for 0<=x<=1 
%  and T = bct0 at x=0, and dT/dx=bct1 at x=1
%
% based on code by Shijie Zhong, modified for Matlab by Thorsten Becker
%
% the Hughes notation is mostly follow here, equation numbers refer to
% the 2000 edition

nnperel = 2;				% number of nodes per element
tol=1e-5;				% relative solution tolerance

levmax = 7;				% number of multigrid levels
base = 2;				% base of multigrid


%
% those are now implemented in the MG solver. if changed, they need to
% be changed there, too. 
%
bct0 = 0;			% temperature boundary condition on left
bct1 = 1.0;	        % flux boundary condition on right

L = 1;					% extent of region

max_iter1 = 4;				% inner iterations
max_iter2 = 40;				% outer iterations

%
% derived quantities
%

for m=1:levmax
    nel=base*2.^(m-1);
    neq=nel+1;
    s(m).nel=nel;			% elements each level
    s(m).node = neq;				% number of nodes
    s(m).neq =  neq;				% number of equations

    s(m).x = (0:s(m).node-1)*L/nel;	% node locations
    %
  % the lm(e,a) matrix will tell which equation in the global system
  % belongs to element e and local node number a
  %
  s(m).lm = zeros(nel,2);
  s(m).lm(:,1)=1:nel;
  s(m).lm(:,2)=s(m).lm(:,1) + 1;
  %
  % set up ien(e,a) array which maps the local node a of element e to
  % the global node number
  %
  s(m).ien = zeros(nel,2);
  s(m).ien(:,1)=1:nel;
  s(m).ien(:,2)=s(m).ien(:,1)+1;
  %
  %  initialize global stiffness matrix and load vector  
  %
  % global arrays
  s(m).stiffness = sparse(neq,neq);		% stiffness matrix (only has bandwidth three in this case)
end
s(levmax).load = zeros(s(levmax).neq,1);
loade= zeros(nnperel,1);		% local load vector

s(levmax).nel

% local
stfm = zeros(nnperel,nnperel);				% local stiffness at element level
xe   = zeros(nnperel,1);			% local node coordinates



if(nnperel ~= 2)
  error('some computations only defined for two nodes per element')
end

%
% assemble stiffness matrix
%
for m = 1:levmax
  nel = s(m).nel;
  neq = s(m).neq;
  for e=1:nel				% loop until element before last
    %
    % get global coordinates from global node numbers of each element
    xe(1) = s(m).x(s(m).ien(e,1));
    xe(2) = s(m).x(s(m).ien(e,2));
    %
    % size of element 
    %
    dx = xe(2)-xe(1);
    %
    % stiffness matrix at elemental level (1.15.3)
    %
    stfm(1,1) = 1.0/dx;
    stfm(1,2) =-1.0/dx;
    stfm(2,1) =-1.0/dx;
    stfm(2,2) = 1.0/dx;
  
    i = s(m).lm(e,1);j= i;
    s(m).stiffness(i,j) = s(m).stiffness(i,j) + stfm(1,1);  
    j = s(m).lm(e,2);
    s(m).stiffness(i,j) = s(m).stiffness(i,j) + stfm(1,2);
    s(m).stiffness(j,i) = s(m).stiffness(j,i) + stfm(2,1);

    if(e < nel)
      i = s(m).lm(e+1,1);
      s(m).stiffness(i,j) = s(m).stiffness(i,j) + stfm(2,2);
    end
    if(m == levmax)
      %
      % elemental load vector, integral over N_a sourcef dx 1.15.6,
      % assuming that sourcef only varies linearly throughout the element
      %
      fac = dx/6;
      loade(1) = (2.0*sourcef(xe(1),L)+    sourcef(xe(2),L))*fac;
      loade(2) = (    sourcef(xe(1),L)+2.0*sourcef(xe(2),L))*fac;
      
      i = s(m).lm(e,1);
      s(m).load(i)=s(m).load(i)+loade(1);
      i = s(m).lm(e,2);
      s(m).load(i)=s(m).load(i)+loade(2);
    end
    s(m).stiffness(neq,neq) = stfm(1,1);% from last element
    %
    % BCs
    %
    i = s(m).lm(1,1); % left
    % temp
    s(m).stiffness(1,2)=0;
  end

  if(m == levmax)			% assign loads only for top level
    %
    % BCs
    %
    i = s(m).lm(1,1); % left
    % temp
    s(m).stiffness(1,2)=0;
    s(m).load(i) = bct0 * s(m).stiffness(1,1);
    %
    i = s(m).lm(nel,2); % right
    s(m).load(i) = s(m).load(i) + bct1;
  end
end
%
% solve
%
[ sol iter res ] = multigrid(s,levmax,tol,max_iter1,max_iter2);
%
% interpolated solution based on elements
%
n=1;
for i=1:s(levmax).nel
  a = s(levmax).ien(i,:); % two global nodes in element 
  for z=-1:.25:1 % element local coordinates
    xi(n) = shape(z)*s(levmax).x(a)';
    ui(n) = shape(z)*sol(a);
    n=n+1;
  end
end


%
% analytical solution for fixed temp on left and fixed flux on right
% and linear increase in heat production
%
ua = bct0 + (1/2*L^2+bct1).*xi - 1/6.*xi.^3;
plot(s(levmax).x,sol,'o',xi,ui,xi,ua,'r')
legend('solution on nodes', 'solution within elements','analytical')

% print results
filename=sprintf('sol.mg.solution');
fp = fopen(filename,'w');
fprintf(fp,'%.4e %.4e\n',s(levmax).x,sol);
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
