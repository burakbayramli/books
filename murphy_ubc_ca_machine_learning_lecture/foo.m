% foo.m
S = 10000;
% normal comment
xs = unifrnd(0,1,S,1);
samples = xs.^3;
%!start printing stuff
fprintf('%f\n', samples)
%!end printing stuff 
Ihat = mean(samples)
se = sqrt(var(samples)/S)
