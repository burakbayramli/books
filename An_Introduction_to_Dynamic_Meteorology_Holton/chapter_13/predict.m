% MATLAB file  predict.m
% Illustration of predictabilty with first order quadratic difference
% equation (13.69).
% Script provides N solutions with slight perturbation of constant a.
close all
clear all
S = 30;                 % total number of iterations
N = input('specify # ensemble members (must be an odd #) = '); 
Nc = (N+1)/2;           % control run (no perturbation)
Y = zeros(S,N);
for n = 1:N
    a = 3.75;           % coefficient in equation
    t = [0:1:S]';
    Y(1,n) = 1.5+.001*(n-Nc)/(Nc);
    for s = 1:S
        Y(s+1,n) = a*Y(s,n) - Y(s,n)^2;
    end
end
figure(1)

for n = 1:N
    plot(t,Y(:,n),'b')
    hold on
end
plot(t,Y(:,Nc),'-k','LineWidth',2)
xlabel('iteration number'), ylabel('Y value')
title('control run: black, perturbation runs: blue')
