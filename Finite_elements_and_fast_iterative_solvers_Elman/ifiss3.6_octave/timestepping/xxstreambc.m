function [fx] = xxstreambc(a,f,xy,bound,t)
%XXSTREAMBC dynamic Dirichlet BC on streamfunction
%   [fgal] = xxstreambc(A,f,xy,bound,t);
%   input
%          A          diffusion matrix without bc's
%          f          rhs vector without bc's
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          t          current time   
%   output
%          fgal        updated rhs vector
%
%   calls function stream_bc
%   IFISS function: DJS; 26 April 2012.
% Copyright (c) 2006 D.J. Silvester, D.F. Griffiths, M. Schneider  

%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
bc=stream_bc(xbd,ybd);
% smoothly grow the hot wall profile
bc=bc*(1-exp(-10*t));
% and adjust the RHS vector accordingly 
fx = f - a(:,bound)*bc; 
fx(bound)=bc;
return
