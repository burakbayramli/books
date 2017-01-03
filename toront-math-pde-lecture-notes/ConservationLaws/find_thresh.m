% X = find_thresh(x,y,thresh)
%
% Given a graph (x,y) which is monotonically decreasing to the
% right of the threshold, this
% routine will find X such that y(X) is approximately equal
% to alpha

function X = find_thresh(x,y,alpha)

% find the point to the left of the desired point
i = min(find(y<alpha))-1;

y_l = y(i);
y_r = y(i+1);
x_l = x(i);
x_r = x(i+1);
X = (-y_l+alpha)*x_r/(y_r-y_l)+(y_r-alpha)*x_l/(y_r-y_l);

