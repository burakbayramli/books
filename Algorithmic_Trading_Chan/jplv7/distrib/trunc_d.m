% PURPOSE: demo of truncated normal draws
%          plots pdf for various truncated normal draws
% 
%---------------------------------------------------
% USAGE: trunc_d
%---------------------------------------------------

n = 1000;
in = zeros(n,1);
iota = ones(n,1);

% generate from -infinity < 2
out = normrt_rnd(in,iota,iota*2);

hist(out);
title('normal 0,1 right-truncated at 2 normal');
pause;

% generate from 0 < infinity
out = normlt_rnd(in,10,10);

hist(out);
title('normal 0,1 left-truncated at zero normal');
pause;

% generate from -1 < +infinity 
% based on mean 1, variance 2
in = ones(n,1);

out = normlt_rnd(in,iota*2,-1*iota);

hist(out);
title('normal 1,2 left-truncated at -1 normal');
pause;

