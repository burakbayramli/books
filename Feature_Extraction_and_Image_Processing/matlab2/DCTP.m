function trans = DCTP(image)
%Computes Discrete Cosine transform. Gives real image as result
%
%  Usage: [new image] = DCTP(image)
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

sum=sum+(image(y,x)*cos((2*y+1)*u*pi/(2*rows))*cos((2*x+1)*v*pi/(2*cols)));
        end
     end
     trans(v,u)=2*sum/(rows*cols);
   end
end
