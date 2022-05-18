%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function c=APP_dd(x)
% x contains the points xi 
% c contains the divided differences
test1=inline('x.*abs(sin(pi*(x)))')
c=test1(x); %warning: "test1" is defined either 
            %in another file or "inline
n=length(x);
for p=1:n-1
    for k=n:-1:p+1
        c(k)=(c(k)-c(k-1))/(x(k)-x(k-p));
    end;
end;


