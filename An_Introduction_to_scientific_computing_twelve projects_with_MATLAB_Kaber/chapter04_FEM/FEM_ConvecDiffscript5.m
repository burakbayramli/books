%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=100;lambda=1;eps=0.01;
x=(0:(n+1))'/(n+1);
A=FEM_ConvecDiffAP1(eps,lambda,n);
X=[ ];Y=[ ];
h=1/(n+1);tab=(1:n)'*h;
for af=1:5
  b=h*cos(af*pi*tab);
  y=A\b;y=[0; y; 0];
  X=[X x];Y=[Y y];
end;
plot(X,Y);
