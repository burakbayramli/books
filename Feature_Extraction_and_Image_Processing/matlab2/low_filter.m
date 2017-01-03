function output = low_filter(image,value)
%Keep points with circle of radius=value
%
%  Usage: [new image] = add(image,number)
%
%  Parameters: image      - array of points 
%              value      - radius of filter
%
%  Author: Mark S. Nixon
%get dimensions
[rows,cols]=size(image); 

%filter the transform
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    if (((y-(rows/2))^2)+((x-(cols/2))^2)-(value^2))>0 output(y,x)=0;   
                   else output(y,x)=image(y,x);
    end
  end
end
