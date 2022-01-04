%                       Program DFT
%
%   Computes the discrete Fourier transform of the function f(x) given
%   in the function mfile f.m. f is assumed given on [-pi, pi].
%   User enters the number N of coefficients desired (must be a power of 2).
%   The program computes the DFT approximations to the real fourier
%   coefficients A_n and B_n for n up to N-1, using the fft of Matlab. 
%   The number of sampling points is 2N, taken from the interval 
%   [-pi pi]. The function g must be defined on [-pi, pi] and
%   given in a mfile g.m.
%   The coefficients A_n are placed in the vector A, and the
%   the coefficients B_n are placed in the vector B. The program
%   further computes the partial sums by adding up the sins and cosines 
%   and placing the answer in the 2N vector sum.  It may be plotted
%   on [-pi, pi].

disp('Enter the number of coefficients to be computed ')
N = input('Must be a power of 2    ')

disp('Program actually calculates coefficients up to N-1 because of quirk in thefinite fourier transform. ')


NN = 2*N;
delx = 2*pi/NN;

% sampling points from the interval [-pi, pi]
xx = -pi : delx :pi - delx;

% The function sampled at the NN points xx.
ff= f(xx);

% Discrete (or finite) Fourier transform produced by matlab
% using the fft routine.
cc = fft(ff)./NN;

%   Shift of coefficients because we are using the interval [-pi, pi]
%   instead of [0, 2pi].
for n = 1:NN
    cc(n) = exp(-(n-1)*pi*i)*cc(n);
end

% calculation of coefficients for real fourier series
A0 = 2*cc(1);
c = cc(2:N);

% c is the vector of complex Fourier coefficients c(1), ..., c(N).  We do
% not use c(N) because it is not very accurate.  In fact, if we doubled N
% we would find that c(N) corresponding to N is the sum  c(N) + c(-N) 
% when the c's are calculated for 2N.
 

A =  2*real(c(1: N-1));
B = -2*imag(c(1:N-1));


% Evaluation of the sum of the fourier series on interval [-pi, pi]
% for plotting.
x = -pi: 2*pi/100: pi;
sum = A0/2;

for n = 1:N-1

     sum = sum + (A(n)*cos(n*x) + B(n)*sin(n*x));
end
