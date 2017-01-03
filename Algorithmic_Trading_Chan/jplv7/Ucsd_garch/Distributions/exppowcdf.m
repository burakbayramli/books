function cdf = exppowcdf(x,nu)
% PURPOSE:
% Cumulants from an exponetial power distribution
% 
% USAGE:
% cdf = exppowpdf(x,nu) 
% 
% INPUTS:
% x   A vector of data
% nu  Either a vector,same length of x or a scalar
% 
% OUTPUTS:
% cdf: Cumulants of an exppow(nu)
% 
% COMMENTS:
% Evaluates the Probabiliy a vector of observations x(Nx1)
% has if drawn from a Exponential Power Dist'n with parameter nu
% f(x)=Kd * exp (-|x|^nu)/((gamma(3/nu)/gamma(1/nu))^0.5)
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
if length(nu)=1;
    nu=ones(size(x)*nu);
end

if (length(x)~=length(nu) | any(nu<1))
    error('Error in arguements')
end


over=x>0
under=x<0
same=x==0

cdf=zeros(size(x));
x(over)
cdf(over)=(1/2)-(gamma(1/nu)*(1-gammainc(x(over).^nu,1/nu)))/(2*nu*gamma(1+(1/nu)))+(1/2);
xunder=-x(under);
cdf(under)=1/2-((1/2)-(gamma(1/nu)*(1-gammainc(xunder.^nu,1/nu)))/(2*nu*gamma(1+(1/nu))));
cdf(same)=.5;


