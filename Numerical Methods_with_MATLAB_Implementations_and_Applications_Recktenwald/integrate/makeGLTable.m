function makeGLTable(n)
% makeGLTable  Create a table of Gauss-Legendre nodes and weights
%              suitable for copy/paste into the GLTable.m function
%
% Synopsis:  makeGLTable
%            makeGLTable(n)
%
% Input:  n = (optional) scalar or vector defining order of rules
%             to be used in constructing table.  Default:  n = 8:10
%             Examples:  n = 3;  n = 4:8;  n = [ 3 5 7 9];
%
% Output:  Table of nodes and weights for Gauss-Legendre quadrature
%          for each value of n.
%
% Note:    Output is formated to be copied and pasted directly
%          into GLTable.m.

if nargin<1,  n=8:10;  end

for k=n
  fprintf('\n  case %d\n',k)
  fprintf('%%      nodes                           weights\n')
  [x,w] = GLNodeWt(k);
  %  Fix case where x should be zero, but isn't due to roundoff
  if rem(round(k),2)~=0    %  True if k is odd
     x(1+fix(k/2)) = 0;    %  Middle index is 1 + k/2 in integer math
  end
  for i=1:length(x)
    fprintf('    x(%d) = %18.15f;     w(%d) = %18.15f;\n',i,x(i),i,w(i))
  end
end
