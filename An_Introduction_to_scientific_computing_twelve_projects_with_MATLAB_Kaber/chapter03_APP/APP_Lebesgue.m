%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function leb=APP_Lebesgue(x)
%Computation of the Lebesgue constant related
%to the points in the array x
n=length(x)-1;
xx=linspace(min(x),max(x),100);%grid (100 points)
y=zeros(size(xx));
for i=1:n+1;
    %calcul de ell_i(x)
    l=zeros(size(x));l(i)=1;cf=polyfit(x,l,n);
    y=y+abs(polyval(cf,xx));
end;
leb=max(y);
