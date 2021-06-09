function a = cvcon(material)
% cvcon  Use switch construct to assign constants in curve fit for cv of fluids
%
% Synopsis:  a = cvcon(material)
%
% Input:    material = (string) name of material;  One of 'CO2','ethane',
%                      'heptane', or 'octane'
%
% Output:   a = vector of coefficients for the curve fit of zero pressure
%               specific heat of the form:
%                 cv = a(1)/T + a(2) + a(3)*T + a(4)*T^2 + a(5)*T^3 + a(6)*T^4

switch lower(material)   %  all comparisons are lower case
  case 'co2'
    a = [8726.361  184.004  1.914025  -1.667825e-3  7.30595e-7  -1.25529e-10];
  case 'ethane'
    a = [26209.109  397.31855  2.0372154  6.3813897e-3  -7.21845581e-6 ...
         2.2048025e-9];
  case 'heptane'
    a = [119252.13  -772.31363  7.4463527  -3.0888167e-3  0.0   0.0];
  case 'octane'
    a = [40859.678  -322.50398  6.6958265  -2.6759063e-3  0.0   0.0];
  otherwise
    error(sprintf('specific heat data for %s not available',material));
end

