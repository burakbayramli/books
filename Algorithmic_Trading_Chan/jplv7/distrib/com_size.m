function [ret, x, aout, bout] = com_size(x,a,b)
% PURPOSE: makes a,b scalars equal to constant matrices size(x)
%          or leaves them alone if they are already common_size
%--------------------------------------------------------------
% USAGE: [ret x aout bout] = com_size(x,a,b)
% where: x = a matrix or vector
%        a = a scalar or matrix
%        b = a scalar or matrix
%--------------------------------------------------------------
% RETURNS:
%        ret = an indicator 0 if common_size, 1 if not
%        x   = input matrix
%        a   = matrix size(x) or input matrix a if already size(x)
%        b   = matrix size(x) or input matrix b if already size(x)
%--------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin ~= 3
error('Wrong # of arguments to com_size');
end;

[n k] = size(x);

ret = 0;

[na ka] = size(a);
[nb kb] = size(b);

if na == 1 & ka == 1
aout = ones(n,k)*a;
else 
 if na ~= n | ka~= k
 ret = 1;
 end;
aout = a;
end;

if nb == 1 & kb == 1
bout = ones(n,k)*b;
else
 if nb ~= n | kb~= k
 ret = 1;
 end;
bout = b;
end;






 