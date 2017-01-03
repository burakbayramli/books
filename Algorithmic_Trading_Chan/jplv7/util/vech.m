function v = vech(x)
% PURPOSE: creates a column vector by stacking columns of x
%          on and below the diagonal
%----------------------------------------------------------
% USAGE:  v = vech(x)
% where:  x = an input matrix
%---------------------------------------------------------
% RETURNS:
%         v = output vector containing stacked columns of x
%----------------------------------------------------------

% Written by Mike Cliff, UNC Finance  mcliff@unc.edu
% CREATED: 12/08/98

if (nargin ~= 1)
  error('Wrong # of arguments to vech'); 
end 

[r,c] = size(x);
v = [];
for i = 1:c
  v = [v;x(i:r,i)];
end
