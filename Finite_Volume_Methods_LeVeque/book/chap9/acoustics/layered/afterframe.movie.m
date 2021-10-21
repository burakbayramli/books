
% to make movie of Figure 9.9
% run with mx = 2400 and dt = 0.1, method(1)=0 so CFL=1.
%

clf
%axes('position',[.1 .1 .8 .4]);
hold on
axis([50 90 -.1 .8])
for i=50:2:90
%axis([0 120 -.1 .8])
%for i=0:2:120
  fill([i i i+1 i+1],[-.1 .8 .8 -.1],[.9 .9 .9])
  end
pp = plot(x,q);

% plot border:
plot([50 90 90 50 50],[-.1 -.1 .8 .8 -.1],'k')
%plot([0 120 120 0 0],[-.1 -.1 .8 .8 -.1],'k')

set(pp,'linewidth',2);
title(['time t =' num2str(t)])

hold off

makeframegif

