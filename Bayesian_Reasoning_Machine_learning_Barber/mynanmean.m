function m = mynanmean(x,dim)
%MYNANMEAN mean of values that are not nan 
nans = isnan(x);
x(nans) = 0;
if nargin==1;
    m = sum(x)./sum(~nans);
else
    m = sum(x,dim)./sum(~nans,dim);
end