function r = specr(A)
%
%   r = specr(A)
%    
%      r = the spectral radius of the matrix A
%

e = eig(A);
r = max(abs(e));
