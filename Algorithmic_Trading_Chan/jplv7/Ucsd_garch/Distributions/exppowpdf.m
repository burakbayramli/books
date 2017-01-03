function prob = exppowpdf(x,nu)
% PURPOSE:
%     Evaluates the Probabiliy a vector of observations x(Nx1)
%     has if drawn from a Exponential Power Dist'n with parameter nu
% 
% USAGE:
%     prob = exppowpdf(x,nu) 
% 
% INPUTS:
%     x   A vector of data
%     nu  Either a vector,same length of x or a scalar
% 
% OUTPUTS:
%     prob: PDF value
% 
% COMMENTS:
%     f(x)=Kd * exp (-|x|^nu)/((gamma(3/nu)/gamma(1/nu))^0.5)
%     KD = inv(2 * gamma (1+(1/nu) ) )
% 
%     Taken from Tadikamalla 1980
%     Included in the ucsd_garch toolbox and the JPL library
%     Requires the JPL toolbox
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

Kd=inv(2*gamma(1+(1/nu)))
prob = Kd*exp(-(abs(x).^nu));
