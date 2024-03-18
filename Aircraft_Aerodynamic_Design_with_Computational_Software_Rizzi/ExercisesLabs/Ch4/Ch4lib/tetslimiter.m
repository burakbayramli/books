% test the limiter
close all
clear all
format compact
n = 100
a = ones(n,1)*linspace(-2,2,n);
b = a';
ep2 = 1e-6
l = ((a+b).*a.*b)./(a.^2+ b.^2 + ep2);
subplot(121)
mesh(a,b,l)
v = a.*(a+b)./(a.^2+b.^2+ep2);
subplot(122)
mesh (a,b,v)
