function gmproc(sigma,alpha)
%GMPROC Plots the autocorrelation and power spectral
%	       functions of a Gauss-Markov process

%Kai Borre 08-08-97
%Copyright (c) by Kai Borre
%$Revision: 1.0$  $Date: 1997/10/17  $

e = exist('gmproc.eps');
if e ~= 0, delete gmproc.eps, end
if nargin == 0, sigma = 1; alpha = 1; end
tau = 0.01:.05:10;
par = alpha*tau;
Rx = sigma^2*exp(-par);
omega = tau;
Sx = 2*sigma^2*alpha./(alpha^2+omega.^2);

subplot('Position',[.05 .1 .4 .3]);
pl1 = plot(tau,Rx,-fliplr(tau),fliplr(Rx));
set(pl1,'LineWidth',1.5)
subplot('POsition',[.55 .1 .4 .3])
pl2 = plot(tau,Sx,-fliplr(tau),fliplr(Sx));
set(pl2,'Linewidth',1.5)
print -deps gmproc
%%%%%%%%%%%%%% end gmproc.m  %%%%%%%%%%%%%%%%%%%%




