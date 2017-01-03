% plot line represented by line plane normal
% J. Kosecka, GMU, March 2002
function [] = plotLineNormal(ln,varargin)

size(varargin);
str = 'white';
if size(varargin,2) == 3 
 xdim = varargin{1};
 ydim = varargin{2};
 ep = varargin{3};
end
if size(varargin,2) == 2
 xdim = varargin{1};
 ydim = varargin{2};
end;
if size(varargin,2) == 4
 xdim = varargin{1};
 ydim = varargin{2};
 ep = varargin{3};
 str = varargin{4};
end;

% ep = ep./ep(3);
lnum = size(ln,2);
for i = 1:lnum
%  ln(:,i)
  p1 = [ln(2,i), -ln(1,i), 0]'/norm([ln(2,i), -ln(1,i), 0]');
  p2 = skew(p1)*ln(:,i);
  if p2(3) ~= 0 
    p2 = p2/p2(3);
end
l = plot([p2(1)-1500*p1(1) p2(1)+1500*p1(1)], [p2(2)-1500*p1(2) p2(2)+1500*p1(2)]);
set(l,'Color',str);
set(l,'LineWidth',1.5);
  % pause
end;

