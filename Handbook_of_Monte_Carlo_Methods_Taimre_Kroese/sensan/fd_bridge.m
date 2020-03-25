%fd_bridge.m
N = 10^6;
a = [1,2,3,1,2];
delta = 10^-3;
u = rand(N,5);
for comp=1:5
  de = zeros(1,5);
  de(comp) = delta;
  L = h1(u,a - de/2);
  R = h1(u,a + de/2);
  c = cov(L,R);
  se = sqrt((c(1,1) + c(2,2) - 2*c(1,2))/N)/delta;
  gr = (mean(R) - mean(L))/delta;
  fprintf('%g pm %3.1e\n',  gr, se);
end
