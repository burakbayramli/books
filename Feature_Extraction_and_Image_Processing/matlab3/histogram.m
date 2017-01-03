function hist = histogram(image)
%Form an image histogram
%
%  Usage: [new histogram] = histogram(image)
%
%  Parameters: image      - array of integers
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%initialise histogram
for i = 1:256
  hist(i)=0;
end
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    level=image(y,x);
    hist(level+1)=hist(level+1)+1;
  end
end
