function [a,f] = streambc(a,f,xy,bound)
%STREAMBC imposes Dirichlet BC on the streamfunction
%   [Agal,fgal] = streambc(A,f,xy,bound);
%   input
%          A          stiffness matrix
%          f          rhs vector
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%   output
%          Agal       stiffness matrix
%          fgal       rhs vector
%
%   calls function stream_bc
%   IFISS function: DJS; 26 April 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx = length(f); nbd=length(bound);
null_col=sparse(nvtx,nbd); null_row=sparse(nbd,nvtx);
Ax=a(1:nvtx,1:nvtx);
fx=f(1:nvtx);
%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
bc=stream_bc(xbd,ybd); 
fx = fx - Ax(:,bound)*bc;
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  fx(bound)=bc; 
a=Ax; f=fx;
return
