function [x_it]=m_st_amgz(x_it,Ast,Q)
%M_ST_AMGZ AMG block preconditioner for Stokes equations
%   x_it = m_st_amgz(x_it,Ast,Q);
%   input
%          x_it         operand for preconditioning operator
%          Ast          (1,1) block, typically d copies of the Laplacian
%          Q            (2,2) block, typically mass matrix or its diagonal
%   output
%          x_it         result of preconditioning operation
%
%   Information needed for preconditioning passed via global variables
%   calls function amg_v_cycle 
%   IFISS function: DJS; 7 December 2009.
% Copyright (c) 2005 D.J. Silvester, J. Boyle
global amg_grid amg_smoother
nv=size(Ast,1); np=size(Q,1); nu=nv/2;
rvx=x_it(1:nu); rvy=x_it(nu+1:nv); rp=x_it(nv+1:nv+np);
% perform V-cycle
zvx=amg_v_cycle(rvx, amg_grid,  amg_smoother);
zvy=amg_v_cycle(rvy, amg_grid,  amg_smoother);
%zp=Q\rp;
zp=diag(Q).\rp;
x_it = [zvx;zvy;zp];
return
