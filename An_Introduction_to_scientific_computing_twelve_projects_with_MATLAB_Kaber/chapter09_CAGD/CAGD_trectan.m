%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function CAGD_trectan(xmi,xma,ymi,yma,color) ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function CAGD_trectan(xmi,xma,ymi,yma,color) 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Display of a rectangle rectangle 
%%   
%%   Input  : xmin,xmax,ymin,ymax vertice coordinates
%%
%%   Output : rectangle display
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
xx(1)=xmi;xx(2)=xma;
xx(3)=xx(2);xx(4)=xx(1);xx(5)=xx(1);
yy(1)=ymi;yy(2)=yy(1);
yy(3)=yma;yy(4)=yy(3);yy(5)=yy(1);
plot(xx,yy,color)
