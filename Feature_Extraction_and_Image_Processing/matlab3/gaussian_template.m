function template = gaussian_template(winsize,sigma)
%Template for Gaussian averaging
%
%  Usage: [template] = gaussian_template(number, number)
%
%  Parameters: winsize    - size of template (odd, integer)
%              sigma      - variance of Gaussian function
%
%  Author: Mark S. Nixon

%centre is half of window size
centre=floor(winsize/2)+1;

%we'll normalise by the total sum
sum=0;

for i=1:winsize
  for j=1:winsize
    template(j,i)=exp(-(((j-centre)*(j-centre))+((i-centre)*(i-centre)))/(2*sigma*sigma));
    sum=sum+template(j,i);
  end
end

template=template/sum;
