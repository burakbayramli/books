%5.3  ilaplacecode.m

%This code was contributed by Andre Weideman

function f = talbotmod(F,t,N)

%   The function f = talbot(F,t,N) implements Talbot's
%   *modified* method for the inversion of the Laplace 
%   Transform.   F is the transform to be inverted, supplied
%   as an inline function of z.   (The code assumes that F is 
%   real when z is real (*).)   N is the number of F evaluations.   
%   The method is based on a 2N panel midpoint rule approximation, 
%   but because of the symmetry (*) it can be reduced to N evaluations 
%   of F.   The parameters sigma, mu, and nu are tuned for optimality
%   in the case where all the singularities of F are on or near the 
%   negative imaginary axis.   In this case a convergence rate of 
%   about exp(-2.7N) can be expected, i.e.,  N = 14 should usually be 
%   sufficient for full precision in MATLAB.  The code does not yet 
%   make provision for multiple values of t. 
%
%   Example of usage:
%    >> F = inline('1./(z.*sqrt(z+1))')
%    >> f = talbotmod(F,1,10) 
%                                      JAC Weideman, September 2006

sigma = -1.2244*N/t; mu = 1.0035*N/t; nu = 0.5272;  % define optimal params
alpha = 0.6407;                                     % new parameter
theta = (2*[0:N-1]+1)*pi/(2*N);                     % equispaced theta vals
z  = sigma + mu*(theta.*cot(alpha*theta) + nu*i*theta);   % modified contour
dz = mu*(cot(alpha*theta)-alpha*theta.*csc(alpha*theta).^2 + nu*i); % dz/dtheta
f  =  1/N*imag(sum(exp(z*t).*F(z).*dz));            % midpoint sum
 
y
