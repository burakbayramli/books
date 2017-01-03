function whitenoi(W)
%WHITENOI Plots the autocorrelation and power spectral
%	       functions of a white noise process

%Kai Borre 08-08-97
%Copyright (c) by Kai Borre
%$Revision: 1.0$  $Date: 1997/10/17  $

e = exist('whitenoi.eps');
if e ~= 0, delete whitenoi.eps, end
if nargin == 0, W = 1; end
tau = 0.01:.05:10;
par = 2*pi*W;
A = 1/par;
arg = par*tau;
Rx = 1/(2*pi)*sin(arg)./arg;
omega = tau;

subplot('Position',[.05 .1 .4 .3]);
pl1 = plot(tau,Rx,-fliplr(tau),fliplr(Rx));
set(pl1,'LineWidth',1.5)
subplot('POsition',[.55 .1 .4 .3])
pl2 = plot([-10 -par -par par par 10],[0 0 A A 0 0]);
set(pl2,'Linewidth',1.5)
print -deps whitenoi
%%%%%%%%%%%%%% end whitenoi.m  %%%%%%%%%%%%%%%%%%%%




