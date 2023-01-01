%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==========================================
% Visualization of the finite elements mesh
%==========================================


function [Iref,CIref]=Show_mail(nfig)

global Nt XYs I123 Reft Nomfic
global txtH

figure(nfig);hold off;
Iref=Find_dif(Reft);

acolor=[1 0 0; 0 1 0; 0 0 1; 1 1 0; 0 1 1; 1 0 1];
colormap(acolor);caxis([1;6]);
cola=char('Red', 'Green','Blue','Yellow','Cyan','Magenta');
ncol=size(acolor,1);
Icol=Iref;Icol=min(Icol,ncol);


xx=XYs(I123',1);xx=reshape(xx,3,Nt);
yy=XYs(I123',2);yy=reshape(yy,3,Nt);
patch(xx,yy,min(Reft',ncol));
axis equal;
title(['Mesh ' Nomfic],'FontUnits','pixels','FontSize',txtH);
drawnow;zoom on;

updatej('Identified domaines (Label/color)')
for i=1:length(Iref)
   updatej(['   ' num2str(Iref(i)) '    ' cola(Icol(i),:)]);
end
CIref=acolor(Icol',:);

