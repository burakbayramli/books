function expr=mysubs1(expr,varargin)

%expr=mysubs1(expr,m,n,...)
%
%  This function substitutes (-1)^m for exp(-i*pi*m) and
%  for exp(i*pi*m), and similarly for exp(-i*pi*n) and
%  exp(i*pi*n), and any other symbols given as inputs.

syms pi i
k=length(varargin);
for jj=1:k
   expr=subs(expr,exp(-i*(pi*varargin{jj})),(-1)^varargin{jj});
   expr=subs(expr,exp(i*pi*varargin{jj}),(-1)^varargin{jj});
end
expr=simplify(expr);

