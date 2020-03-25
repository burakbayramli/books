% mcint.m
c = (2*pi)^(3/2);
H = @(x) c*sqrt(abs(sum(x,2)));
N = 10^6; alpha = 0.05;
x = randn(N,3); y = H(x);
mY = mean(y); sY = std(y);
RE = sY/mY/sqrt(N);
z = icdf('norm',1-alpha/2,0,1);
fprintf('Estimate = %g, CI = (%g, %g)\n', ...
		    mY, mY*(1- z*RE), mY*(1 + z*RE))
