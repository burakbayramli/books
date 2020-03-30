% PlotSurf(xe,ye,Pf,neval,exact,maxerr,fview,caption)
% Generates plot of surface Pf false colored by the
% maximum error abs(Pf-exact)
% fview defines the view, and the caption is displayed.
function PlotSurf(xe,ye,Pf,neval,exact,maxerr,fview,caption)
% Plot surface
figure
Pfplot = surf(xe,ye,reshape(Pf,neval,neval),...
              reshape(abs(Pf-exact),neval,neval));
set(Pfplot,'FaceColor','interp','EdgeColor','none')
set(gca,'Fontsize',14)
xlabel('x','FontSize',14);
ylabel('y','FontSize',14);
zlabel('z','FontSize',14,'Rotation',0);
%zlim([-0.2,1.2])
[cmin cmax] = caxis;
caxis([cmin-.25*maxerr cmax]);
view(fview); 
colormap hsv
vcb = colorbar('vert');
ylim(vcb,[0 maxerr])
set(get(vcb,'YLabel'),'String','Error','FontSize',14)
%title(caption)
