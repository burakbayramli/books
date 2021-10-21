
% run with mx=480 and CFL = 0.8, method(1)=1
% after running with fine grid to set qtrue

clf
axes('position',[.1 .1 .8 .4]);
mq=1;
plotframe1
axis([0 120 -1 1])
print coarse1.eps
query

clf
plotframe1
clf
axis([50 90 -.1 .8])
hold on
for i=50:2:90
  fill([i i i+1 i+1],[-.1 .8 .8 -.1],[.9 .9 .9])
  end
plot(x,q,'o');

% plot border:
plot([50 90 90 50 50],[-.1 -.1 .8 .8 -.1],'k')

pp = plot(xtrue,qtrue);
set(pp,'linewidth',2);
title(' ')
print coarse2.eps

