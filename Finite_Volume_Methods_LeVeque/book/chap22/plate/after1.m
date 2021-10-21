axis([0 1 -.01 .01])

if (PlotType ~= 2)

 cmax = max(caxis);
 caxis([-cmax cmax]);
 rybcolormap
 colorbar

 cfactor = 2;  % factor to coarsen data by before plotting displaced solid
 dd = .5;
 if ~exist('dd','var')
    dd = input(' input dd = ');
    end

 fname(6) = 'd';
 fid = fopen(fname);
 data = fscanf(fid,'%g',[2,(mx+1)*(my+1)])';
 xdisp = reshape(data(:,1),mx+1,my+1);
 ydisp = reshape(data(:,2),mx+1,my+1);
 [xloc,yloc] = meshgrid(x,y);
 

 %dd = 100*t;
 xloc = xloc' + dd*xdisp;
 yloc = yloc' + dd*ydisp;

 figure(3)
 clf
 axes('position',[.1 .1 .8 .3])

 hh = surf(xloc',yloc',0*qaug',0*qaug');
 view(2)
 colormap([1 1 1])  % all white
 caxis([-cmax cmax]);
 axis([-.1 1.2 -.02 .02])

if cfactor>1 
  % plot grid:
  set(hh,'EdgeColor','none');
  hold on
  indi = 1:cfactor:size(xloc,1);
  indj = 1:cfactor:size(xloc,2);
  plot3(xloc(indi,indj)',yloc(indi,indj)',0*xloc(indi,indj)','k')
  plot3(xloc(indi,indj),yloc(indi,indj),0*xloc(indi,indj),'k')
  hold off
  end

%title(['deformation of plate at time t = ' num2str(t)])
text(-.15,0,['t = ' num2str(t)])
axis off

 %makeframegif

end 
