% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Example_2_16
% ~~~~~~~~~~~~~~~~~~~
%{
  This program uses the bisection method to find the three roots of
  Equation 2.204 for the earth-moon system.

  m1  - mass of the earth (kg)
  m2  - mass of the moon (kg)
  r12 - distance from the earth to the moon (km)
  p   - ratio of moon mass to total mass
  xl  - vector containing the low-side estimates of the three roots
  xu  - vector containing the high-side estimates of the three roots
  x   - vector containing the three computed roots

  User M-function required: bisect
  User subfunction requred: fun
%}
% ----------------------------------------------

clear all; clc

%...Input data:
m1  = 5.974e24;
m2  = 7.348e22;
r12 = 3.844e5;

xl  = [-1.1  0.5  1.0];
xu  = [-0.9  1.0  1.5];
%...End input data

p   = m2/(m1 + m2);

for i = 1:3
    x(i) = bisect(@fun, xl(i), xu(i));
end

%...Output the results
output

return

% ~~~~~~~~~~~~~~~~~
function f = fun(z)
% ----------------- 
%{
  This subroutine evaluates the function in Equation 2.204
 
  z - the dimensionless x-coordinate
  p - defined above
  f - the value of the function

%}
% ~~~~~~~~~~~~~~~~~
f = (1 - p)*(z + p)/abs(z + p)^3 + p*(z + p - 1)/abs(z + p - 1)^3 - z;
end %fun

% ~~~~~~~~~~~~~
function output
% ~~~~~~~~~~~~~
%{
  This function prints out the x-coordinates of L1, L2 and L3
  relative to the center of mass.
%}
%...Output to the command window:
fprintf('\n\n---------------------------------------------\n')
fprintf('\n For\n')
fprintf('\n   m1 = %g kg', m1)
fprintf('\n   m2 = %g kg', m2)
fprintf('\n  r12 = %g km\n', r12)
fprintf('\n the 3 colinear Lagrange points (the roots of\n')
fprintf(' Equation 2.204) are:\n')
fprintf('\n L3: x = %10g km    (f(x3) = %g)',x(1)*r12, fun(x(1)))
fprintf('\n L1: x = %10g km    (f(x1) = %g)',x(2)*r12, fun(x(2)))
fprintf('\n L2: x = %10g km    (f(x2) = %g)',x(3)*r12, fun(x(3)))
fprintf('\n\n---------------------------------------------\n')

end %output

end %Example_2_16
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~