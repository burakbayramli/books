
axis square
axis([-1 1 -1 1])

if PlotType==1
  rybcolormap
  caxis([-.1 .1])
  colorbar
  end

hold on
hh = plot([.0 .0],[-1 0],'r');
set(hh,'LineWidth',2);
hh = plot([.0 1],[0 .55],'r');
set(hh,'LineWidth',2);
hold off

