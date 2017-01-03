function thresholded = threshold(image,level)
%Set points above level to white, others to black
%
%  Usage: [new image] = threshold(image)
%
%  Parameters: image      - array of points
%              threshold  - brightness level
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%set points to white if above threshold level, to black otherwise
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    if image(y,x)>level thresholded(y,x)=255;
                   else thresholded(y,x)=0;
    end
  end
end
