function out=cdf_laplace(x)

if x<=0
    out=exp(x)/2;
else
    out=1-exp(-x)/2;
end