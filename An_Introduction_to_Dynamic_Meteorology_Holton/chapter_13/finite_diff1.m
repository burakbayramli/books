% MATLAB file: finite_diff1.m
% Program to illustrate error of centered difference approximation
% for second derivative of sin(kx).
clear all
close all
x = linspace(0,32,33);
dx=1;
nwaves = input('enter number of grid intervals per wavelength (32 maximum) ')
k = 2*pi/nwaves;
F = sin(k*x);
DF = -k^2*sin(k*x);
for m = 2:32
    fdiff(m) = (F(m+1)-2*F(m)+F(m-1))/dx^2;
end
fdiff(1) = (F(2)-2*F(1)+F(32))/dx^2;
fdiff(33) = fdiff(1);
figure(1)
plot(x,DF,'b-'), xlabel('x'), ylabel('amplitude')
title('Derivative (solid), Finite Difference (dashed)')
hold on
plot(x,fdiff,'--')
figure(2)
plot(x,(fdiff-DF)), xlabel('x'), ylabel('amplitude')
title('finite difference minus derivative')
max(fdiff-DF) 