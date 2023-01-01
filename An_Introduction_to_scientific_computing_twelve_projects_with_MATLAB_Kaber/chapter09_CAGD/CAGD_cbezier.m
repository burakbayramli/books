%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [X,Y]=CAGD_cbezier(T,XP,YP);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [X,Y]=CAGD_cbezier(T,XP,YP)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Construction of points P(T) of a Bézier curve
%%   by using de Casteljau algorithm.
%%   
%%   Input : T sampling values
%%           XP, YP control points coordinates
%%
%%   Ouput : X, Y coordinates of P(T) in R^2
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=size(T,2); 
X=zeros(n,1);Y=zeros(n,1);
for k=1:n
t=T(k);
[x,y]=CAGD_casteljau(t,XP,YP);
X(k)=x;Y(k)=y;
end
