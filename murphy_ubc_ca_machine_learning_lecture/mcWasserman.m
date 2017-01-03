% mcWasserman.m
% Demo of Monte Carlo integration from Wasserman p405
S = 10000;
xs = unifrnd(0,1,S,1);
samples = xs.^3;
Ihat = mean(samples)
se = sqrt(var(samples)/S)
