function f = errintg(x,param)
% Error function integrand
% Inputs
%    x       Value where integrand is evaluated
%    param   Parameter list (not used)
% Output
%    f       Integrand of the error function
f = exp(-x^2);
return;
