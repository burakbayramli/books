function  rho = H2Odensity(T,units)
% H2Odensity  Density of saturated liquid water
%
% Synopsis:   rho = H2Odensity
%             rho = H2Odensity(T)
%             rho = H2Odensity(T,units)
%
% Input:  T  = (optional) temperature at which density is evaluated
%              Default: T = 20C. If units='F', then T is degrees F
%      units = (optional) units for input temperature, Default = 'C'
%              units = 'C' for Celsius, units = 'F' for Fahrenheit
%
% Output:  rho = density, kg/m^3 if units='C'; lbm/ft^3 if units='F'

% Notes:  Use 4th order polynomial curve fit of data in Table B.2
%         (Appendix B) of "Fundamentals of Fluid Mechanics",
%         B. R. Munson, et al., 2nd edition, 1994, Wiley, NY

if nargin<1
  rho = 998.2;  return;    %  Density at 20 C
elseif nargin==1
  units='C';               %  Default units are C
end

% --- Convert to degrees C if necessary
if upper(units)=='F'
  Tin = (T-32)*5/9;     %  Convert F to C; don't change input value
elseif upper(units) == 'C'
  Tin = T;
else
  error(sprintf('units = ''%s'' not allowed in H20density',units));
end

% --- Make sure temperature is within range of curve fit
if Tin<0 | Tin>100
  error(sprintf('T = %f (C) is out of range for curve fit',Tin));
end

% --- Curve fit coefficients
c = [ 1.543908249780381441e-05  -5.878005395030049852e-03 ...
      1.788447211945859774e-02  1.000009926781338436e+03];

rho = polyval(c,Tin);     % Evaluate polynomial curve fit
if upper(units)=='F'
  rho = rho*6.243e-2;     % Convert kg/m^3 to lbm/ft^3
end
