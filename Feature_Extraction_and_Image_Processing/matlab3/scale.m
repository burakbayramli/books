function scaled = scale(image,gain,level)
%Scale an image and add a value
%
%  Usage: [new image] = scale(image,gain,level)
%
%  Parameters: image      - array of points
%              gain       - multiplying constant
%              level      - added constant
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%multiply by gain and add level to image points
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    scaled(x,y)=floor(gain*image(x,y)+level);
  end
end
