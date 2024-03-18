% CFL for RK Gary
close all
clear all
ark = [0.0695 0.1602 0.2898 0.5060 1];
n = length(ark)
p = @(z) (1 + z*ark(5).*(1+z*ark(4).*(1+z*ark(3).*(1+z*ark(2).*(1+ark(1)*z)))));
y = linspace(0,4,200);
plot(y,abs((p(1i*y))))