function taxicab()

as = 0.1:0.01:5;
bs = [1, 1,  1, 0.001];
Ks = [2, 1, 0.1, 0.001];

figure(1);clf
b=1;K=2;
ps = pareto_pdf(as, 1, 2);
plot(as, ps, 'linewidth', 2)
set(gca, 'ylim', [-0.1 2])
title(sprintf('b=%5.3f, K=%5.3f', 1, 2))

figure(2);clf
ms   = as;
b=1; K=2;
for mi=1:length(ms)
  D = [0, ms(mi)];
  ev(mi) = pareto_evidence(D, b, K);
end
plot(ms, ev, 'linewidth', 2)
plot(ms, ev, 'linewidth', 2)
set(gca, 'ylim', [-0.1 0.6])
return


for i=1:4
  subplot(2,2,i)
  K = Ks(i);
  b = bs(i);
  ps = pareto_pdf(as, b, K);
  plot(as, ps, 'linewidth', 2)
  set(gca, 'ylim', [-0.1 2])
  title(sprintf('b=%5.3f, K=%5.3f', b, K))
end

figure(2);
ms = as;
N = 2;
for i=1:4
  subplot(2,2,i)
  K = Ks(i);
  b = bs(i);
  for mi=1:length(ms)
    D = [0, ms(mi)];
    ev(mi) = pareto_evidence(D, b, K);
  end
  plot(ms, ev, 'linewidth', 2)
  set(gca, 'ylim', [-0.1 0.6])
  title(sprintf('b=%5.3f, K=%5.3f', b, K))
end

function p = pareto_pdf(a, b, K)

p = K*b^K ./ (a.^(K+1));
ndx = find(a < b);
p(ndx) = 0;


function ev = pareto_evidence(D, b, K)

m = max(D);
N = length(D);
if m <= b
  ev =  K/( (N+K) * b^N );
else
  ev = (K * b^K)/( (N+K).*m.^(N+K));
end


function ev = pareto_evidence2(m, N, b, K)

%m = max(D);
%N = length(D);
ev = zeros(1, N);
ndx = find(m <= b);
ev(ndx) = K/( (N+K) * b^N );
ndx = find(m < b);
ev(ndx) = (K * b^K)/( (N+K).*m(ndx).^(N+K));

