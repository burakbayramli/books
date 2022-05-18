%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function CAGD_tsbezier(X,Y,color,XP,YP,shift,pchar,pcolor,ptrait);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  function CAGD_tsbezier(X,Y,color,XP,YP,pchar,pcolor,ptrait)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Display of a Bézier curve with control points
%%   
%%   Input : X, Y sampling points coordinates
%%           XP, YP control points coordinates
%%           shift  use to shift character display
%%           color  curve color
%%           pchar  control point character
%%           pcolor control polygon color
%%           ptrait control polygon line type
%%
%%   Ouput : Display the curve and control points
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
line=strcat(pcolor,ptrait);
plot(X,Y,color,XP,YP,line)
np=size(XP,2);
for k=1:np
kk=k-1;
char=int2str(kk);
P=strcat(pchar,char);
epsx0=0.10;epsy0=0.15;ks=1;
if (k>shift) ks=2; end
epsx=epsx0*ks;epsy=epsy0*ks;
if (k==np) epsx=0.;epsy=-0.20;   end
if (k==np) epsx=epsx+0.20; epsy=-0.20;  end
fs=14;text(XP(k)+epsx,YP(k)+epsy,P,'FontSize',fs);
%% fprintf('\n k   %d',k);fprintf('\n ks  %d',ks);
end
