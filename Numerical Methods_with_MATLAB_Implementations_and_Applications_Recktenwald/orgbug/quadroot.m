function r = quadroot(a,b,c)
% quadroot   Roots of quadratic equation and demo of keyboard command
%
% Synopsis:  r = quadroot(a,b,c)
%
% Input:     a,b,c = coefficients of  a*x^2 + b*x + c = 0
%
% Output:    r = column vector containing the real or complex roots

% See Chapter 4, Unavoidable Errors in Computing, for a discussion
% of the formula for r(1) and r(2)
d = b^2 - 4*a*c;
if d<0
  fprintf('Warning in function QUADROOT:\n');
  fprintf('\tNegative discriminant\n\tType "return" to continue\n');
  keyboard;
end
q = -0.5*( b + sign(b)*sqrt(b^2 - 4*a*c) );

r = [q/a; c/q];  % store roots in a column vector
