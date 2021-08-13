function Jnst = newtonbc(J,xy,bound)
%NEWTONBC imposes Dirichlet BC on Jacobian
%   Jnst = newtonbc(J,xy,bound);
%   input
%          J          Jacobian velocity matrix
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%   output
%          Jnst       Jacobian velocity matrix
%
%   IFISS function: DJS, HCE; 30 August 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

nvtx = length(xy(:,1));
nu = nvtx*2; nbd=length(bound);
null_col=sparse(nu,nbd); %null_row=sparse(nbd,nu);
Jnst=J;

%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
%% impose boundary condition
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1); 

%Procedure is equivalent to the two commented lines below
%It is more efficient because only columns are referenced
%Jnst(:,bound)=null_col;  Jnst(bound,:)=null_row;
%Jnst(:,nvtx+bound)=null_col;  Jnst(nvtx+bound,:)=null_row;

Jt = Jnst';
Jt(:,bound) = null_col;
Jt(:,nvtx+bound) = null_col;
Jnst = Jt';
Jnst(:,bound) = null_col;
Jnst(:,nvtx+bound) = null_col;

Jnst = Jnst + [spdiags(dA,0,nvtx,nvtx),sparse(nvtx,nvtx); ...
               sparse(nvtx,nvtx),spdiags(dA,0,nvtx,nvtx)];
