function averaged = ave33(image)
%New image point brightenss = average of 3*3 region in image
%
%  Usage: [new image] = ave33(image)
%
%  Parameters: image      - array of points
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black (0)
averaged(1:rows,1:cols)=0;


%then form the average of the nine points
for x = 2:cols-1 %address all columns except border
  for y = 2:rows-1 %address all rows except border
    averaged(y,x)=floor((image(y-1,x-1)+image(y-1,x)+image(y-1,x+1)...
                          +image(y,x-1)+image(y,x) + image(y,x+1)...
                        +image(y+1,x-1)+image(y+1,x)+image(y+1,x+1))/9);
  end
end
