function X=HarmonicOscillator(T,X0);

%% function for harmonic oscillator
%% linear oscillator when b=0;
%% nonlinear oscillator when b<>0.

omega=1;  b=1;

%% Construction of matrix
A=[ 0 1 ; -omega^2 0];
%% Construction of AX0
X=A*X0;
%% Nonlinearity taken into account.
X=X*(1+b*X0(1)*X0(2));


