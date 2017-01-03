function horizontal_edges = edge_y(image)
%Find edges by vertical differencing
%
%  Usage: [new image] = edge_y(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black
horizontal_edges= zeros(rows,cols);

%then form the difference between vertical successive points
for x = 1:cols %address all columns
  for y = 1:rows-1 %address all rows except border
    horizontal_edges(y,x)=abs(image(y,x)-image(y+1,x));
  end
end
