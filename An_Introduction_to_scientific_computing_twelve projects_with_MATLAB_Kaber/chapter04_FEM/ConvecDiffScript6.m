%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% Ou l'on compare P1 et P1 + bullesn=
n=10;eps=0.01;lambda=1;
f = inline('ones(size(x))');          
A1=ConvecDiffAP1(eps,lambda,n);
A2=ConvecDiffAP2(eps,lambda,n);
a=A2(2:2:2*n+1,2:2:2*n+1);
b=A2(2:2:2*n+1,1:2:2*n+1);
c=A2(1:2:2*n+1,2:2:2*n+1);
d=A2(1:2:2*n+1,1:2:2*n+1);
T=ConvecDiffAP1(1,0,n);  %matrice du probleme sans convection
                         %avec epsilon=1
A12=a-b*inv(d)*c-A1;     %difference entre les matrices P1
                         %et P1 +bulles
A12./T
