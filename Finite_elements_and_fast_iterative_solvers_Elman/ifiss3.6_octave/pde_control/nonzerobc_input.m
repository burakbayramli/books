function [a,f] = nonzerobc_input(a,f,xy,bound,bc_nodes)
%NONZEROBC_INPUT imposes specific Dirichlet boundary conditions
%   [Agal,fgal] = nonzerobc_input(A,f,xy,bound,bc_nodes);
%   input
%          A          stiffness matrix
%          f          rhs vector
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          bc_nodes   vector providing BCs (and zeros at interior nodes)
%   output
%          Agal       stiffness matrix
%          fgal       rhs vector
%
%   does not call function specific_bc; instead vector is provided as input
%   IFISS function: DJS, JWP; 27 June 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage, J.W. Pearson
nvtx = length(f); nbd=length(bound);
null_col=sparse(nvtx,nbd); null_row=sparse(nbd,nvtx);
Ax=a(1:nvtx,1:nvtx);
fx=f(1:nvtx);
%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
bc=bc_nodes; % specific_bc(xbd,ybd);
fx = fx - Ax(:,bound)*bc;
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  fx(bound)=bc; 
a=Ax; f=fx;
return

