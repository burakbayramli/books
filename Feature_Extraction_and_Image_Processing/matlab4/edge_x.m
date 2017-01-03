function vertical_edges = edge_x(image)
%Find edges by horizontal differencing
%
%  Usage: [new image] = edge_x(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black
vertical_edges = zeros(rows,cols);
%this is equivalent to vertical_edges(1:rows,1:cols)=0

%then form the difference between horizontal successive points
for x = 1:cols-1 %address all columns except border
  for y = 1:rows %address all rows
    vertical_edges(y,x)=abs(image(y,x)-image(y,x+1));
  end
end
