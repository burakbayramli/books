
%
% A simple script illustrating inverse z transform by power series codes
% File-name: pseries.m
b1 =	[1 0.481199 1];
b2 =	[1 1.474597 1];
a1 =	[1 0.052921 0.83173];
a2 =	[1 -0.304609 0.238865];
sos =	[b1 a1; b2 a2];
[b, a] = sos2tf(sos);
m = length(b);
r = b;
for n=1:m
   [h(n), r] = deconv(r, a);
   h
   r
   r = [r(2:m) 0];
end  


