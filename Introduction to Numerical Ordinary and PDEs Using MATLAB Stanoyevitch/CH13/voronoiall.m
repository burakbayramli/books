function voronoiall(x,y)
% M-file for EFR 13.3
% inputs:  two vectors x and y of the same size giving, respectively, 
% the x- and y- coordinates of a set of distinct points in the plane
% outputs:  none, but a graphic will be produced of the Voronoi regions
% corresponding to the point set in the plane, including the unbounded
% regions
n=length(x);
xbar = sum(x)/n; ybar = sum(y)/n; %centroid of points
md = max(sqrt(x-xbar).^2 + sqrt(y-ybar).^2); %maximum distance of points 
%                                              to centroid
mdx = max(abs(x-xbar)); mdy = max(abs(y-ybar)); %max x- and y- distances
%                                              to averages
% We create additional points that lie in a circle of radius 3md about 
% (xbar, ybar).  We deploy them with angular gaps of 1 degree, this will
% be suitable for all practical purposes.  
xnew=x;, ynew=y;
for k = 1:360
    xnew(n+k)=xbar+3*md*cos(k*pi/180); 
    ynew(n+k)=ybar+3*md*sin(k*pi/180);
end
voronoi(xnew,ynew)
axis([min(x)-mdx/2 max(x)+mdx/2  min(y)-mdy/2  max(y)+mdy/2])

    