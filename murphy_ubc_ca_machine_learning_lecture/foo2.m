%!start1
% bar.m
S = 10000;
% normal comment
xs = unifrnd(0,1,S,1);
samples = xs.^3;

% ...skip printing stuff...
%!stop
fprintf('%f\n', samples)
%!start2

Ihat = mean(samples)
se = sqrt(var(samples)/S)
rand(2,2)

% ...skip printing stuff...
%!stop
disp('junk')
%!start3

rand(3,3)
rand(4,4)
