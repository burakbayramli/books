
% make the figure for the book showing the interface.

hold on
axis square
%axis off
%plot([-1 1 1 -1 -1], [-1 -1 1 1 -1])
hh = plot([.0 .0],[-1 0],'r');
set(hh,'LineWidth',2);
hh = plot([.0 1],[0 .55],'r');
set(hh,'LineWidth',2);
text(.4,-.6,'rhor')
text(-.6,0,'rhol')
hold off

