function equalised = equalise(image)
%Nonlinear histogram equalisation
%
%  Usage: [new image] = equalise(image)
%
%  Parameters: image      - array of integers
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image); 

%specify range of levels
range=255;

%and the number of points
number=cols*rows;

%initialise the image histogram
for i=1:256
  hist(i)=0;
end;

%work out the histogram
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    hist(image(y,x)+1)=hist(image(y,x)+1)+1;
  end
end;

%evaluate the cumulative histogram
sum=0;
for i=1:256
  sum=sum+hist(i);
  cumhist(i)=floor(sum*range/number);
end

%map using the cumulative histogram
for x = 1:cols %address all columns
  for y = 1:rows %address all rows
    equalised(y,x)=cumhist(image(y,x));
  end
end

