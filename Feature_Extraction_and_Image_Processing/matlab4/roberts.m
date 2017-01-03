function edges = roberts(image)
%Find all edges by roberts cross operator
%
%  Usage: [new image] = roberts(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black
edges = zeros(rows,cols);

for x = 1:cols-1 %address all columns except right border
  for y = 1:rows-1 %address all rows except bottom border
    %top right minus bottom left point
    M_plus=abs(image(y,x+1)-image(y+1,x));
    %top left minus bottom right point
    M_minus=abs(image(y,x)-image(y+1,x+1));
    %return maximum
    edges(y,x)=max(M_plus,M_minus);
  end
end
