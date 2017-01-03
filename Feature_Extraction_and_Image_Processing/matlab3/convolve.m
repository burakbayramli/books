function convolved = convolve(image,template)
%New image point brightenss convolution of template with image
%  Usage: [new image] = convolve(image,template of point values)
%
%  Parameters: image      - array of points
%              template   - array of weighting coefficients
%
%  Author: Mark S. Nixon

%get image dimensions
[irows,icols]=size(image);

%get template dimensions
[trows,tcols]=size(template);

%set a temporary image to black
temp(1:irows,1:icols)=0;

%half of template rows is
trhalf=floor(trows/2); 

%half of template cols is
tchalf=floor(tcols/2); 

%then convolve the template
for x = trhalf+1:icols-trhalf %address all columns except border
  for y = tchalf+1:irows-tchalf %address all rows except border
    sum=0;
    for iwin = 1:trows %address template columns
      for jwin = 1:tcols %address template rows
        sum=sum+image(y+jwin-tchalf-1,x+iwin-trhalf-1)*template(jwin,iwin);
      end
    end
    temp(y,x)=sum;
  end
end

%finally, normalise the image
convolved=normalise(temp);
