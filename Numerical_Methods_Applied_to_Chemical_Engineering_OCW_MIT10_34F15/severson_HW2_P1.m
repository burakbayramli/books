clear 
close all
clc

%Input Matrix
M = [-10^-4 10^-2 0; 10^-4 -(10^-2 + 10^-3) 0; 0 10^-3 0];

%Using MATLAB's Schur decomposition to decompose the matrix
[U, T] = schur(M)