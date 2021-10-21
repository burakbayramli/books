hold on
plot(xtrue,qtrue)
axis([0 1 -.5 1.5])

% RJL's additional stuff to title and print out eps files for book:

set(gca,'fontsize',20)
str = input('title? ');
htitle = title(str);
set(htitle,'fontsize',20)
prfile = input('print to file advcomp_?');
if ~isempty(prfile)
  eval(['print  advcomp_' prfile ' -deps'])
  end
hold off
