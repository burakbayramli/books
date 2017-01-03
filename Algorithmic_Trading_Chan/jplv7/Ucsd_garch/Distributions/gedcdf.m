function cdf = gedcdf(x,nu)
% PURPOSE:
%  Evaluates the Probabiliy a vector of observations x(Nx1)
%  has if drawn from a Generalzed Error Dist'n  with parameter nu
% 
% 
% USAGE:
% cdf = gedcdf(x,nu)
% 
% INPUTS:
%    x  - Data
%    nu - Shape parameter(scalar or size(t))
% 
% OUTPUTS:
%   cdf - Cumulants of a GED
% 
% COMMENTS:
% The exponential power distn with variance normalized to 1
% 
% 
% Taken from Tadikamalla 1980
% 
% f(x)=Kd * exp (-|x|^nu)
% KD = inv(2 * gamma (1+(1/nu) ) )
% 
% Included in the ucsd_garch toolbox and the JPL library
% Requires the JPL toolbox
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001




beta=((2.^(-2./nu)).*(gamma(1./nu))./(gamma(3./nu))).^(0.5);
scale=nu./(2.^(1+1./nu).*beta.*gamma(1./nu));
cdfcore=ones(size(x));

over=x>0;
under=x<0;
same=x==0;
cdf=zeros(size(x));


cdfcore=2.^(1./nu).*beta.*scale;

if ~isempty(over) & max(over)>0;
cdf(over)=cdfcore*((nu.*gamma(1+1./nu)-((gamma(1./nu)-gamma(1./nu).*gammainc((1/2).*((1./beta).^nu).*(x(over).^nu),1./nu))))./nu)+.5;
end
if ~isempty(under) & max(under)>0
xunder=-x(under);
cdf(under)=.5-cdfcore.*((nu.*gamma(1+1./nu)-((gamma(1./nu)-gamma(1./nu).*gammainc((1/2).*((1./beta).^nu).*(xunder.^nu),1./nu))))./nu);
end

if ~isempty(same) & max(same)>0
cdf(same)=.5;
end










