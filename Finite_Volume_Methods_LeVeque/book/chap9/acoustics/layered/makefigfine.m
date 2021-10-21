
% run with mx = 1200 and dt = 0.1, method(1)=0 so CFL=1.
%

clf

axes('position',[.1 .1 .8 .4]);
mq=1;
plotframe1
axis([0 120 -1 1])
print fine1.eps
query

clf
plotframe1
clf
hold on
axis([50 90 -.1 .8])
for i=50:2:90
  fill([i i i+1 i+1],[-.1 .8 .8 -.1],[.9 .9 .9])
  end
pp = plot(x,q);

% plot border:
plot([50 90 90 50 50],[-.1 -.1 .8 .8 -.1],'k')

set(pp,'linewidth',2);
title(' ')
print fine2.eps

hold off

qtrue = q;
xtrue = x;
