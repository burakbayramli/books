function edges = prewitt(image)
%derive edges by 3*3 Prewitt operator
%
%  Usage: [new image] = prewitt(image)
%
%  Parameters: image      - array of points
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black (0)
edges(1:rows,1:cols)=0;

for x = 2:cols-1 %address all columns except border
  for y = 2:rows-1 %address all rows except border
    %apply Mx template
    x_mag=image(y-1,x-1)+image(y-1,x)+image(y-1,x+1)...
         -image(y+1,x-1)-image(y+1,x)-image(y+1,x+1);
    %apply My template
    y_mag=image(y-1,x-1)+image(y,x-1)+image(y+1,x-1)...
         -image(y-1,x+1)-image(y,x+1)-image(y+1,x+1);
    %evaluate edge magnitude (vector length)
    edges(y,x,1)=sqrt((x_mag*x_mag)+(y_mag*y_mag));
    %evaluate edge direction (vector inclination)
    if x_mag==0 edges(y,x,2)=sign(y_mag)*1.5708;
       else edges(y,x,2)=atan(y_mag/x_mag);
       end
  end
end
