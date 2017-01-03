function edges = edge(image)
%Find all edges by first order differencing
%
%  Usage: [new image] = edge(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black
edges= zeros(rows,cols);

%then form the difference between horizontal and vertical points
for x = 1:cols-1 %address all columns
  for y = 1:rows-1 %address all rows except border
    edges(y,x)=abs(2*image(y,x)-image(y+1,x)-image(y,x+1));
  end
end
