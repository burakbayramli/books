function l = linevec(v,w,c)
%
%  linevec(v,w)
%
%  Draw a line of color c where v and w are the endpoints
%  if c is omitted, color is black
%
%   See also ARROWVEC, TEXTVEC, VECTOR

vv = [v;w];

if nargin < 3, c = 'k'; end

switch size(v,2),
  case 2,
    l = line(vv(:,1),vv(:,2),'color',c);
  case 3,
    l = line(vv(:,1),vv(:,2),vv(:,3),'color',c);
  otherwise error('vector must be two or three-dimensional')
end
