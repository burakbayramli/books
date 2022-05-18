%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function cfp=APP_equiosc(x)
np2=length(x);n=np2-2;
A=zeros(n+1,n+1);
for i=1:n+1
    for j=0:n
        A(i,j+1)=x(i+1)^j -(-1)^i*x(1)^j;
    end;
    b(i)=f(x(i+1)) -(-1)^i*f(x(1));
end;
cfp=A\b';

