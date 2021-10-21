

 axis([0 2.0 0 1])

if UserVariable==1
   title(['sigma11 + sigma22 at t = ',num2str(t)])
  elseif mq==3
   title(['sigma12 at t = ',num2str(t)])
   end

if PlotType ~= 2
 if PlotType ~=3
 cmax = max(abs(range(q)));
 if cmax>0
   caxis([-cmax cmax]);
   end
 rybcolormap
 colorbar
 end

 cfactor = 1;  % factor to coarsen data by before plotting displaced solid
 %cfactor = 6;  % factor to coarsen data by before plotting displaced solid
 dd = 5;       % amplification factor for deformation
 if ~exist('dd','var')
    dd = input(' input dd = ');
    end

 % read in deformation:
 [dxdy,tdxdy] = readamrdata(2,Frame,'./','aux');
 xdisp = reshape(dxdy.data(1,:),mx,my);
 ydisp = reshape(dxdy.data(2,:),mx,my);


 [xloc,yloc] = meshgrid(xcenter,ycenter);
 z = xloc>.5 & xloc<1.5-dx & yloc>.4 & yloc<.6-dy;
 %z = xloc>.5 & xloc<1.0-dx & yloc>.2 & yloc<.6-dy;
 z = double(z);
 

 xloc = xloc' + dd*xdisp;
 yloc = yloc' + dd*ydisp;

 figure(3)
 clf
 %axes('position',[.1 .1 .8 .533])
 myaxes([-.1 2.1 -.1 1.1])


 % plot embedded object:
 hh = surf(xloc',yloc',zeros(size(z)),z);
 view(2)

 % yellow and red:
 % colormap([1 1 .8;1 .1 .1])

 colormap([1 1 1;.5 .5 .5])
 axis([-.1 2.1 -.1 1.1])

if cfactor>1 
  % plot grid:
  set(hh,'EdgeColor','none');
  hold on
  indi = 1:cfactor:size(xloc,1);
  indj = 1:cfactor:size(xloc,2);
  plot3(xloc(indi,:)',yloc(indi,:)',0*xloc(indi,:)','k')
  plot3(xloc(:,indj),yloc(:,indj),0*xloc(:,indj),'k')
  hold off
  end

title(['deformation at t = ',num2str(t)])

end  % if PlotType ~= 2
