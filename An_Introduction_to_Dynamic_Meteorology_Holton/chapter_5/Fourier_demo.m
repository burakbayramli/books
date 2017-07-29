% MATLAB file Fourier_demo.m
% Program to demonstrate Fourier series approximation to 
% Function exp(x)  in the domain  -pi< x < pi
% (See Hildebrand, Advanced Calculus for Applications)
% Student should try different number of modes to see how
% the approximation improves with added modes
%
close all
clear all
N = input('type number of Fourier modes to include  ')
x = linspace(-pi,pi,30);
f = exp(x);
A0 = sinh(pi)/pi;
F = ones(size(x))*A0;
for j=1:N
    An(j) = 2*sinh(pi)/pi*cos(j*pi)/(j^2+1);
    Bn(j) = -2*sinh(pi)/pi*j*cos(j*pi)/(j^2+1);
end
for j=1:N
    F = F + An(j).*cos(j*x) +Bn(j).*sin(j*x);
end
plot(x,f)
hold on
plot(x,F,'o'), title('Circles show Fourier series fit')
xlabel('x'), ylabel('f(x)')
axis([ -pi pi -4 24])
