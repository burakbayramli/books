function added = add(image,value)
%New image point brightness = old brightness + value
%
%  Usage: [new image] = add(image,number)
%
%  Parameters: image      - array of points 
%              value      - added brightness
%
%  Author: Mark S. Nixon
%get dimensions
[rows,cols]=size(image); 

%add value to image points
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    added(y,x)=image(y,x)+value;
  end
end
