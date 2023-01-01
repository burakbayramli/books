%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=APP_interpole(c,x,g)
%compute the interpolation of the function f on the grid g 
%knowing the divided differences c computed at the points x
n=length(c);
y=c(n)*ones(size(g));
for k=n-1:-1:1
    y=c(k)+y.*(g-x(k));
end;
