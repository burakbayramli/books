function logged = logn(image)
%New image point brightenss = logarithm of old
%
%  Usage: [new image] = logn(image)
%
%  Parameters: image      - array of points >0
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%then take the logarithm
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    logged(y,x)=floor(log(image(y,x)));
  end
end
