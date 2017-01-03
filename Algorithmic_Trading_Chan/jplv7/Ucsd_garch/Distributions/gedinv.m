function num = gedinv(x,nu)
% PURPOSE:
% Evaluates the Probabiliy a vector of observations x(Nx1)
% has if drawn from a Generalzed Error Dist'n  with parameter nu
% 
% 
% USAGE:
% num = gedinv(x,nu)
% 
% INPUTS:
% x - Data
% nu - Shape parameters
% 
% OUTPUTS:
% num - Inverse CDF values form a GED
% 
% COMMENTS:
% The exponential power distn with variance normalized to 1
% 
% f(x)=Kd * exp (-|x|^nu)
% KD = inv(2 * gamma (1+(1/nu) ) )
% 
% Taken from Tadikamalla 1980
% 
% Included in the ucsd_garch toolbox and the JPL library
% Requires the JPL toolbox
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001




high=5;
low=-5;
for i=1:20
    mid=(high+low)/2;
    h=gedcdf(high,nu);
    m=gedcdf(mid,nu);
    l=gedcdf(low,nu);
    a=[h,m,l];
    if any(x==a)
        num=a(find(x==a));
    elseif x<h & x>m
        low=mid;    
    else
        high=mid; 
    end
end
num=mid;