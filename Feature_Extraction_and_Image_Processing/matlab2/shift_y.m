function shifted = shift_y(image,value)
%Shifts an image hirizontally, and wraps round
%
%  Usage: [new image] = shift_y(image,amount)
%
%  Parameters: image      - array of points 
%              value      - amount of shift
%
%  Author: Mark S. Nixon
%get dimensions
[rows,cols]=size(image); 

%and shift it
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    shifted(y,x)=image(y,mod(x+value-1,cols)+1);
  end
end
