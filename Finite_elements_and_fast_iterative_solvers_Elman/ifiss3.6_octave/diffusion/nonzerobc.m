function [a,f] = nonzerobc(a,f,xy,bound)
%NONZEROBC imposes Dirichlet boundary condition 
%   [Agal,fgal] = nonzerobc(A,f,xy,bound);
%   input
%          A          stiffness matrix
%          f          rhs vector
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%   output
%          Agal       stiffness matrix
%          fgal       rhs vector
%
%   calls function specific_bc
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx = length(f); nbd=length(bound);
null_col=sparse(nvtx,nbd); null_row=sparse(nbd,nvtx);
Ax=a(1:nvtx,1:nvtx);
fx=f(1:nvtx);
%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
bc=specific_bc(xbd,ybd); 
fx = fx - Ax(:,bound)*bc;
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  fx(bound)=bc; 
a=Ax; f=fx;
return
