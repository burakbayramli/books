

if PlotType==1
  yrbcolormap
  caxis([1 3])
  colorbar
  showgridlines
  end

if PlotType==4
  rpsoln = readm('rpsoln',4);
  rpsoln(1,1) = -100;
  rpsoln(end,1) = 100;
  hold on
  plot(t*rpsoln(:,1),rpsoln(:,2),'r')
  axis([-2 2 .5 3.5])
  hold off
  end
