%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=APP_mapLeg(coef,n,x)
z=legendre(0,x);L=z(1,:);
y=coef(1)*L;
if n>0
   z=legendre(1,x);L=z(1,:);
   y=y+coef(2)*L;
   for k=2:n
       z=legendre(k,x);L=z(1,:);
       y=y+coef(k+1)*L;
   end;
end;
