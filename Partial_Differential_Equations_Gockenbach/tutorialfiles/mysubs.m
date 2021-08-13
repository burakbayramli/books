function expr=mysubs(expr,varargin)

%expr=mysubs(expr,m,n,...)
%
%  This function substitutes 0 for sin(m*pi) and (-1)^m
%  for cos(m*pi), and similarly for sin(n*pi) and cos(n*pi),
%  and any other symbols given as inputs.

syms pi
k=length(varargin);
for j=1:k
   expr=subs(expr,sin(varargin{j}*pi),0);
   expr=subs(expr,cos(varargin{j}*pi),(-1)^varargin{j});
end
expr=simplify(expr);

