function pdf = logt_pdf (x)
% PURPOSE: pdf of the logistic distribution at x
%---------------------------------------------------
% USAGE: pdf = logt_cdf(x)
% where: x = a vector or scalar argument 
%---------------------------------------------------
% RETURNS:
%        pdf = the pdf of the logistic distribution      
% --------------------------------------------------
% SEE ALSO: logt_cdf, logt_pdf, logt_inv, logt_rnd
%---------------------------------------------------

% NOTE: Written by KH (Kurt.Hornik@ci.tuwien.ac.at)
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu
      

  if (nargin ~= 1)
    error('Wrong # of arguments to logt_pdf');
  end;

  cdf = logt_cdf (x);
  pdf = cdf .* (1 - cdf);
  
