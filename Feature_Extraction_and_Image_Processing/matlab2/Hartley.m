function trans = Hartley(image)
%Computes Hartley transform. Gives real image as result
%
%  Usage: [new image] = Hartley(image)
%
%  Parameters: image      - array of points 
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%compute transform
for u = 1:cols %address all columns
  for v = 1:rows %address all rows
    sum=0;
    for x = 1:cols %address all columns 
      for y = 1:rows %address all rows 
      sum=sum+(image(y,x)*(cos(2*pi*(u*x+v*y)/rows)+sin(2*pi*(u*x+v*y)/cols)));
      end
    end
    trans(v,u)=sum/cols;
  end
end
