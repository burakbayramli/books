function bar2(v,p,color)
% bar2(v,p,color)
% 
% This function uses a modification of the Matlab function
% "bar", Copyright (c)1984-94 by The MathWorks, Inc. to plot
% an amplitude distribution or a power density spectrum.
%
% v     =vector of bar-center values.
% p     =vector of amplitude or power density values, same length.
% color =color of plot (optional), for example, 'k'.
line_width=2;
x=row_vec(v);
y=row_vec(p);
if(nargin==3),
   c=color;
end
if(length(x)~=length(y)),
    error('bar2: inputs must be vectors of the same length.');
end
%Note copyright below.
%BAR	Bar graph.
%	BAR(Y) draws a bar graph of the elements of vector Y.
%	BAR(X,Y) draws a bar graph of the elements of vector Y at
%	the locations specified in vector X.  The X-values must
%	be in ascending order.  If the X-values are not evenly spaced, the
%	interval chosen is not symmetric about each data point.  Instead,
%	the bars are drawn midway between adjacent X-values.  The endpoints
%	simply adopt the internal intervals for the external ones needed.
%
%	If X and Y are matrices the same size, one bar graph per column 
%	is drawn.
%
%	[XX,YY] = BAR(X,Y) does not draw a graph, but returns vectors
%	X and Y such that PLOT(XX,YY) is the bar chart.
%
%	See also STAIRS, HIST.
%
%	BAR(X,'linetype') or BAR(X,Y,'linetype') uses the plot linetype
%	specified.  See PLOT for details.
%	Copyright (c) 1984-94 by The MathWorks, Inc.
               
if nargin == 1
  if isstr(x)
	error('First argument must be numeric.');
  end
  if min(size(x))==1, y = x(:); else y = x; end
  [n,m] = size(y);
  x = [1:n]'*ones(1,m);
  c = '-';
elseif nargin >= 2
  if isstr(x)
	error('First argument must be numeric.');
  end
  if isstr(y),
    c = y;
    if min(size(x))==1, y = x(:); else y = x; end
    [n,m] = size(y);
    x = [1:n]'*ones(1,m);
  else
    if min(size(y))==1, y = y(:); end
    [n,m] = size(y);
    if min(size(x))==1, x = x(:)*ones(1,m); end
    if nargin~=3, c = '-'; end
  end
end

nn = 5*n;
yy = zeros(nn+1,m);
xx = yy;
yy(3:5:nn,:) = y;
yy(4:5:nn,:) = y;

notequal = max(abs(diff(diff(x)))) > max(max(abs(x)))*sqrt(eps);

if max(diff(x))==0, notequal=[]; end % Special case 

if isempty(notequal), % Scalar case and special case
    delta = 1;
    t = x - 0.5*delta;
    xx(1:5:nn,:) = t;
    xx(2:5:nn,:) = t;
    xx(3:5:nn,:) = t;
    xx(4:5:nn,:) = t + delta;
    xx(5:5:nn,:) = t + delta;
    xx(nn+1,:) = xx(nn,:)+0.1*delta;
elseif ~notequal
    delta = ones(n,1) * (max(x) - min(x)) / (n-1);
    t = x - 0.5*delta;
    xx(1:5:nn,:) = t;
    xx(2:5:nn,:) = t;
    xx(3:5:nn,:) = t;
    xx(4:5:nn,:) = t + delta;
    xx(5:5:nn,:) = t + delta;
    xx(nn+1,:) = xx(nn,:);
else    % spacing is unequal - do the best you can
    dx = diff(x)/2;
    xx(1:5:nn+1,:) = [x;x(n,:)] - [dx(1,:);dx;dx(n-1,:)];
    xx(2:5:nn+1,:) = x - 0.9*[dx(1,:);dx];
    xx(3:5:nn+1,:) = xx(2:5:nn+1,:);
    xx(4:5:nn+1,:) = x + 0.9*[dx;dx(n-1,:)];
    xx(5:5:nn+1,:) = xx(4:5:nn+1,:);
    xx(nn,:) = x(n,:) + 0.9*dx(n-1,:);
    xx(nn+1,:) = xx(nn,:);
end

if nargout == 0
    cax = newplot;
    next = lower(get(cax,'NextPlot'));
    hold_state = ishold;
    h=plot(xx,yy,c);
    set(h,'linewidth',line_width);
    axy = axis;
    hold on;
    h=plot([axy(1) axy(2)],[0 0],'-');
    set(h,'color',get(gca,'xcolor'))
    if notequal
        plot(x,zeros(size(x)),'*b')
    end
    if ~hold_state, set(cax,'NextPlot',next); end
else
    xo = xx;
    yo = yy;
end
