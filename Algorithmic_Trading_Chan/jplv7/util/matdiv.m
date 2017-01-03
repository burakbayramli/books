function out = matdiv(x,y)
% PURPOSE: performs matrix division even if matrices
%          are not of the same dimension, but are row or
%          column compatible
%---------------------------------------------------
% USAGE: result = matdiv(x,y)
% where:    x,y = two matrices (not of the same dimension
%                 but are row or column compatible)
%---------------------------------------------------
% RETURNS: result = x ./ y where x and y are row or column compatible
% --------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% Unioutersity of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


[rx,cx] = size(x);
[ry,cy] = size(y);

if (cx == cy) & (rx == ry);
  out = x ./ y;
elseif (cx == cy) & (rx == 1)
	out = y ./ repmat(x,ry,1);
elseif (cx == cy) & (ry == 1)
	out = x ./ repmat(y,rx,1);
elseif (rx == ry) & (cx == 1);
	out = y ./ repmat(x,1,cy);
elseif (rx == ry) & (cy == 1);
	out = x ./ repmat(y,1,cx);
else;
   error('matdiv: non-conformable in row or column dimension')
end
