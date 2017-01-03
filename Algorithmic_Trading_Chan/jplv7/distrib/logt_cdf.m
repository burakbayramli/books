function cdf = logt_cdf (x)
% PURPOSE: cdf of the logistic distribution
%---------------------------------------------------
% USAGE: cdf = logt_cdf(x)
% where: x = a vector or scalar argument 
%---------------------------------------------------
% RETURNS:
%        cdf = the cdf of the logistic distribution      
% --------------------------------------------------
% SEE ALSO: logt_cdf, logt_pdf, logt_inv, logt_rnd
%---------------------------------------------------

% NOTE: Written by KH (Kurt.Hornik@ci.tuwien.ac.at)
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu
    

  if (nargin ~= 1)
    error('Wrong # of arguments to logt_cdf');
  end;

  cdf = 1 ./ (1 + exp (- x));
  
