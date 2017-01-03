function t = textvec(v,i,c)
%
%  textvec(v,i,c)
%
%  Write number i at position v using color c
%  v is a 2 or 3 dimensional vector
%  if c is omitted, color is black
%
%   See also ARROWVEC, LINEVEC, VECTOR

if nargin < 3, c = 'k'; end

is = num2str(i);

switch size(v,2)
  case 2,
     t = text(v(1),v(2),is,'color',c,'HorizontalAlignment','center');
  case 3,
     t = text(v(1),v(2),v(3),is,'color',c,'HorizontalAlignment','center');
  otherwise error('vector must be two or three-dimensional')
end
