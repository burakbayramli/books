%discinvtrans.m
p = [0.2,0.3,0.1,0.05,0.35];
N = 10^5;
[dummy,x]=histc(rand(1,N),[0,cumsum(p)]);
freq = hist(x,1:5)/N

