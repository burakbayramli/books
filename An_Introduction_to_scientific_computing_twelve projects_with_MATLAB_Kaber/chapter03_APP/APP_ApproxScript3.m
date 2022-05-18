%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%Lagrange basis
n=20;
x=(0:n)'/n;
i=round(n/2);
twon=n;           % a quoi ca sert?
g=(0:twon)'/twon;   % a quoi ca sert?
y=zeros(size(x));
y(i)=1;
cf=polyfit(x,y,n);
fprintf('l_[n/2](0)=%f\n',polyval(cf,0));
