% PlotErrorSlices(xe,ye,ze,Pf,exact,ne,xslice,yslice,zslice,caption)
% Generates slice plot of volume error abs(Pf-exact)
% xslice,yslice,zslice define the range and number of slices,
% and the caption is displayed.
function PlotErrorSlices(xe,ye,ze,Pf,exact,ne,...
                         xslice,yslice,zslice,caption)
% Plot slices for error
figure
errorplot = slice(xe,ye,ze,reshape(abs(Pf-exact),ne,ne,ne),...
                  xslice,yslice,zslice);
set(errorplot,'FaceColor','interp','EdgeColor','none')
daspect([1 1 1])
view(3); axis([0 1 0 1 0 1])
[cmin cmax] = caxis;
caxis([cmin-.25*cmax cmax])
colormap hsv
vcb = colorbar('vert');
ylim(vcb,[0 cmax])
set(get(vcb,'YLabel'),'String','Error','FontSize',14)
title(caption)
