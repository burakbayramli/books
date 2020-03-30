% PlotSlices(xe,ye,ze,Pf,neval,xslice,yslice,zslice,caption)
% Generates slice plot of volume Pf
% xslice,yslice,zslice define the range and number of slices,
% and the caption is displayed.
function PlotSlices(xe,ye,ze,Pf,neval,xslice,yslice,...
                    zslice,caption)
% Plot slices
figure
pfit = slice(xe,ye,ze,reshape(Pf,neval,neval,neval),...
             xslice,yslice,zslice);
set(pfit,'FaceColor','interp','EdgeColor','none')
daspect([1 1 1])
view(3); axis([0 1 0 1 0 1]) 
vcb = colorbar('vert');
set(get(vcb,'YLabel'),'String','Function value','FontSize',14)
title(caption)
