% PlotError2D(xe,ye,Pf,exact,maxerr,neval,fview,caption)
% Generates plot of abs error for surface Pf, i.e., abs(Pf-exact)
% fview defines the view, and the caption is displayed.
function PlotError2D(xe,ye,Pf,exact,maxerr,neval,fview,caption)
% Plot maximum error
figure
errorplot = surf(xe,ye,reshape(abs(Pf-exact),neval,neval));
set(errorplot,'FaceColor','interp','EdgeColor','none')
[cmin cmax] = caxis;
caxis([cmin-.25*maxerr cmax])
view(fview);
colormap hsv
vcb = colorbar('vert');
ylim(vcb,[0 maxerr])
set(get(vcb,'YLabel'),'String','Error','FontSize',14)
title(caption)
