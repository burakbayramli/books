clc
echo on

% This code starts by creating the Fourier matrix.
% It contains powers of the complex number omega.
% This number is an nth root of 1. Choose n = 4 so omega = i:

n = 4;
i = sqrt(-1);
omega = exp(-2*pi*i/n);

% The j,k entry of the Fourier matrix is the j*k power of omega.
j = 0:n-1;
k = j';
F = omega .^ (k*j)
% This is a unitary matrix if we divide by sqrt(n). We don't.
% The discrete Fourier transform of a vector x is F*x.

x = [1  1  -1  -1]
transform = F*x'
% press any key to see circulant matrices
pause
clc

% A circulant matrix has n numbers looping down its diagonals
%
% [c(1) c(2) c(3) c(4)
%  c(4) c(1) c(2) c(3)
%  c(3) c(4) c(1) c(2)
%  c(2) c(3) c(4) c(1)]
%
%  If the first row is the vector c, flip it to construct the matrix:

c = [2 3 4 5];
d = fliplr(c);
firstcol = [c(1) d(1:length(c)-1)];
circulant = toeplitz(firstcol,c)

% CONVOLUTION Multiplying circulant times a vector v gives the
% periodic convolution of firstcol with v
%
% EIGENVECTORS All circulant matrices are diagonalized by F!
% The columns of F are the eigenvectors of the circulant.
% So F' * circulant * F should show the eigenvalue matrix.
% We must divide our F and F' by sqrt(n) to get the right lambda.
%
lambda = F' * circulant * F / n

% Experiment: Show that the product of circulants is another circulant.

echo off
