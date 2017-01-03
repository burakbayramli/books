function averaged = ave(image,winsize)
%New image point brightness = average of points in window
%
%  Usage: [new image] = ave(image,window size)
%
%  Parameters: image      - array of points 
%              winsize    - size of window (odd, integer)
%
%  Author: Mark S. Nixon

%get dimensions
[rows,cols]=size(image);

%set the output image to black
averaged(1:rows,1:cols)=0;

%half of template is
half=floor(winsize/2); 

%then form the average by adding up points in the window
for x = half+1:cols-half %address all columns except border
  for y = half+1:rows-half %address all rows except border
    sum=0;
    for iwin = 1:winsize %address template columns
      for jwin = 1:winsize %address template rows
        sum=sum+image(y+jwin-half-1,x+iwin-half-1);
      end
    end
    %and then dividing by the number of points in the window
    averaged(y,x)=floor(sum/(winsize*winsize));
  end
end
