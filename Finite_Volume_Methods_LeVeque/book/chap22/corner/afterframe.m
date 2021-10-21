if PlotType==1
  rybcolormap
  caxis([-.1 .1])
  colorbar
  end


% plot corner geometry:

hold on
hh = plot([.0 .0],[-1 0],'r');
set(hh,'LineWidth',2);
hh = plot([.0 1],[0 .55],'r');
set(hh,'LineWidth',2);
hold off


axis([-1 1 -1 1])
axis square

