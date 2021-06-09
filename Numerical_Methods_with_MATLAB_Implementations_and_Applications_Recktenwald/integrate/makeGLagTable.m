function makeGLagTable(n)
% makeGLagTable  Create a table of Gauss-Laguerre nodes and weights
%                suitable for copy/paste into the GLagTable.m function
%
% Synopsis:  makeGLagTable
%            makeGLagTable(n)
%
% Input:  n = (optional) scalar or vector defining order of rules
%             to be used in constructing table.  Default:  n = 8:10
%             Examples:  n = 3;  n = 4:8;  n = [ 3 5 7 9];
%
% Output:  Table of nodes and weights for Gauss-Laguerre quadrature
%          for each value of n.
%
% Note:    Output is formated to be copied and pasted directly
%          into GLagTable.m.

if nargin<1,  n=8:10;  end

for k=n
  fprintf('\n  case %d\n',k)
  fprintf('%%      nodes                           weights\n')
  [x,w] = GLagNodeWt(k);
  for i=1:length(x)
    fprintf('    x(%d) = %18.15f;     w(%d) = %24.15e;\n',i,x(i),i,w(i))
  end
end
