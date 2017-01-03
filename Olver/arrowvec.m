function l = arrowvec(v,w,c)
%
%  arrowvec(v,w)
%
%  Draw a directed line of color c where v and w are the endpoints
%  if c is omitted, color is black
%
%   See also LINEVEC, TEXTVEC, VECTOR


if nargin < 3, c = 'k'; end

s = .8; t = .05; u = .025;
d = w - v; n = d/norm(d);
p = [n(2), -n(1)];

a = v + s * d;
b1 = a - t * n + u * p;
b2 = a - t * n - u * p;

linevec(v,w,c);
linevec(a,b1,c);
linevec(a,b2,c);
