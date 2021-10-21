hold on
axis([-1 4 -0.5 2.5])

% exact solution:
plot([-5 t t 5],[2 2 0 0],'--')

% modified equation solution:
if (t>0)
  cfl = 0.8;
  beta = .5 * dx * (1-cfl);
  xx = -1:.01:4;
  v = erfc((xx-t)/sqrt(4*beta*t));
  plot(xx,v)
  end

title(['time t = ' num2str(t)])
hold off

