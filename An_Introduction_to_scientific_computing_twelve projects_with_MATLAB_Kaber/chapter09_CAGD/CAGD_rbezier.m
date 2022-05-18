%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [inter,xmi,xma,ymi,yma]=rbezier(XP1,YP1,XP2,YP2);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Intersection of two Bézier curves
%%   Rectangular hulls computation - Intersection check
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
[xmin1,xmax1,ymin1,ymax1]=CAGD_drectan(XP1,YP1);
[xmin2,xmax2,ymin2,ymax2]=CAGD_drectan(XP2,YP2);
xmi=max(xmin1,xmin2);xma=min(xmax1,xmax2);
ymi=max(ymin1,ymin2);yma=min(ymax1,ymax2);
inter=0;seuil=1.e-06;
if ( xma-xmi > seuil )
   inter = inter + 1 ;
end
if ( yma-ymi > seuil )
   inter = inter + 1 ;
end
%% fprintf('\n Intersection : %d %f %f %f %f ',inter,xmi,xma,ymi,yma);
