function inverted = invert(image)
%Subtract image point brightness from maximum
%
%  Usage: [new image] = invert(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%find the maximum
maxi = max(max(image));

%subtract image points from maximum
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    inverted(y,x)=maxi-image(y,x);
  end
end
