function Apx = pressurebc(Ap,n_null)
%PRESSUREBC fixes singularity in Laplacian matrix
%   Apx = pressurebc(Ap,n_null);
%   input
%          Ap      singular Laplacian matrix
%          n_null  index of "pegged" dof 
%   output
%          Apx     reduced non-singular Laplacian matrix
%
%   IFISS function: DJS; 23 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
np = length(Ap(1,:)); 
fprintf('fixing singularity in pressure matrix... ')
minor = [1:n_null-1,n_null+1:np]';
Apx = Ap(minor,minor);
fprintf('done \n')
return
