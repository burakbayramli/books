function Ax = mg_zerobc(A,xy,bound)
%MG_ZEROBC imposes zero boundary conditions
%   Ax = mg_zerobc(A,xy,bound)
%   input
%           A       matrix with no boundary conditions imposed
%           xy 
%           bound   index set of Dirichlet boundary nodes
%   output  
%           Ax      modified matrix with boundary conditions imposed
%
%   IFISS function: DJS, HCE; 21 March, 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nvtx = length(A(1,:)); nbd=length(bound);
null_col=sparse(nvtx,nbd); null_row=sparse(nbd,nvtx);
Ax=A(1:nvtx,1:nvtx);
% impose boundary condition
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  
