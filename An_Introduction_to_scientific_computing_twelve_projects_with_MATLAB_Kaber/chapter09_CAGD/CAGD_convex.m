%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function h=CAGD_convex(X,Y);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function h=CAGD_convex(X,Y) 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Evaluation of the Bézier curve convexity
%%   
%%   Input  : XP, YP control points coordinates            
%%
%%   Output : h approximation for curve convexity
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=size(X,2);h=0.;seuil=1.e-06;
for k=2:n-1
   xx=X(k-1)-X(k+1);yy=Y(k-1)-Y(k+1);
   if ( abs(xx) > seuil ) 
     yy=Y(k-1)*(X(k)-X(k+1))-Y(k+1)*(X(k)-X(k-1));
     yk=yy/xx;
     h=max(h,abs(yk-Y(k)));
   else if ( abs(yy) > seuil ) 
     xx=X(k-1)*(Y(k)-Y(k+1))-X(k+1)*(Y(k)-Y(k-1));
     xk=xx/yy;
     h=max(h,abs(xk-X(k)));
   end
   end
end
