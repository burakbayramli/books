function  [rho,mu,sigma,a] = H2OProps(T,units)
% H2OProps  Thermophysical properties of saturated liquid water
%
% Synopsis:   [rho,mu,sigma,a] = H2OProps
%             [rho,mu,sigma,a] = H2OProps(T)
%             [rho,mu,sigma,a] = H2OProps(T,units)
%
% Input:   T = (optional) temperature (deg. C) at which properties are
%              to be evaluated.  By default T = 20 C. If units='BG' then
%              T is assumed by be degrees Farhrenheit
%          units = (optional) string to indicate the system of units
%                  units = 'SI' for System International (kg-m-s) units,
%                  units = 'BG' for British Gravitational (lbm-ft-s) units
%
% Output: rho   = density, kg/m^3 or lbm/ft^3
%         mu    = dynamic viscosity, kg/(m*s) = Pa*s or lbm/(ft*s)
%         sigma = surface tension coefficient, N/m or lbf/ft
%         a     = sound speed, m/s or ft/s

% Notes:  Curve fits obtained from data in Table B.2 (Appendix B) of
%         "Fundamentals of Fluid Mechanics", B.R. Munson, et al.,
%         second edition, 1994, Wiley and Sons, NY

if nargin<1         %  return properties at 20 C w/out evaluating curve fit
  rho = 998.2;  mu = 1.002e-3;  sigma = 7.28e-2;  a = 1481;
  return
elseif nargin==1
  units='SI';
end

if units=='BG'
  Tin = (T-32)*5/9;    %  convert deg F to deg C
else
  Tin = T;
end

if Tin<0 | Tin > 100
  error(sprintf('T = %f (deg C) is out of range for property curve fits',Tin));
end

% --- store polynomial curve fit coefficients
pr = [ 1.543908249780381441e-05 -5.878005395030049852e-03  1.788447211945859774e-02 ...
       1.000009926781338436e+03];
pm = [-2.562680382013827576e-09  5.770146662394522851e-07 -4.696938055563929843e-05 ...
       1.752087056845374156e-03];
ps = [ 1.543571532552364284e-09 -4.980068956164041499e-07 -1.323675995296498716e-04 ...
       7.559001971334708414e-02];
pa = [ 9.300881201391979637e-05 -4.122379026510247790e-02  4.579691847346554212e+00 ...
       1.404337855186843626e+03];

% --- evaluate the polynomials
rho   = polyval(pr,Tin);   mu = polyval(pm,Tin);
sigma = polyval(ps,Tin);    a = polyval(pa,Tin);

if units=='BG'
  rho = rho*6.243e-2;  mu = mu*2.089e-2;  sigma = sigma*6.851e-2;  a = a*3.281;
end
