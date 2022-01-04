
%                          Program dirch
%
%  This program solves the Dirichlet problem in the unit disk for given
%  boundary data f. The program calculates the approximate Fourier
%  coefficients of g using the Matlab fast fourier transform and then
%  sums the terms n = 0,. . . , 63. User must provide an array smart 
%  function mfile called f.m for the boundary data.  f must be periodic
%  of period 2pi.



NN= 128;
M = 21;

r = 0:.05:1;
theta= 0: 2*pi/NN :2*pi;
X = r'* cos(theta);
Y = r'* sin(theta);


% Calculate the Fourier coefficients of the boundary data g


ff = f(theta(1:NN));

cc = fft(ff)/NN;

A0 = 2*cc(1);

% reindex the complex fourier coefficients  as c(1), ..., c(NN/2-1)
c = cc(2:NN/2); 

% We use coefficients only up to NN/2-1 because NN/2 coefficient is not
%  usable, and by the periodicity cc(n) = cc(-n) for n > NN/2.
N = NN/2 -1;


%calculation of real coefficients

A =  2*real(c(1: N));
B = -2*imag(c(1: N));


% the constant term in the solution

u = .5*A0*ones(size(X));

for n = 1:N
     u = u +(r.^n)'*(A(n)*cos(n*theta) + B(n)*sin(n*theta));
end
     
surf(X,Y,u)





