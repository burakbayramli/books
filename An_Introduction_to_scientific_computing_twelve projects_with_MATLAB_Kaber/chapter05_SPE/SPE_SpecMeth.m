%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 5.4 
%% Resolution of -u"+cu=f using  Galerkin spectral method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
clear
m=16;   % degree of  Legendre approximation
s=m+1;  % degree of Gauss quadrature for the right hand side.
global c
c=30.; 
% Construction of the matrix
A=zeros(m,m);
for i=1:m
 A(i,i)=(i*(i+1))^2*(1./(0.5+i)+ 4.*c/((2.*i+1.)*(2*i-1)*(2*i+3))) ;
end
for i=1:m-2
 A(i,i+2)=-2*c*i*(i+1)*(i+2)*(i+3)/((2*i+1)*(2*i+3)*(2*i+5));
end
for i=3:m
 A(i,i-2)=-2*c*i*(i+1)*(i-2)*(i-1)/((2*i-1)*(2*i-3)*(2*i+1));
end
% Construction of the right hand side vector
[absc,weights]=SPE_xwGauss(s);
t=SPE_fbe(absc); u=t.*weights; 
LX0=ones(s,1); 
LX1=absc;
C=zeros(m+2,1); 
C(1)=t'*weights/2; C(2)=3*u'*LX1/2;
for k=2:m+1
  % computes $f_k$ in c(k+1)
  % computes values of  $L_k$ at integration abscissa
  % kL_k=(2k-1)xL_{k-1} -(k-1)L_{k-2}
    LX2=((2*k-1)*absc.*LX1-(k-1)*LX0)/k;
    C(k+1)=(2*k+1)*u'*LX2/2;
    LX0=LX1;
    LX1=LX2;   
end
B=zeros(m,1);
for i=1:m
  B(i)=2*i*(i+1)*(C(i)/(2*i-1)-C(i+2)/(2*i+3))/(2*i+1);
end
% Solves the linear system
U=A\B;
%
% Change of basis
% (1-x^2)L_i'=(i(i+1)/(2i+1)).(L_{i-1} - L_{i+1}
UN=zeros(1,m+2);
for k=1:m
  CC=(k+1)*k*U(k)/(2*k+1);
  UN(k)=UN(k)+CC;
  UN(k+2)=UN(k+2)-CC;
end
%
% Computes approximate solution and error
%
n=100; 
xa=linspace(-1,1,n); 
y=SPE_LegLinComb(xa,UN);
es=norm(y-SPE_special(xa),inf);
%
% Computes difference finite solution 
mdf=50; h=2/mdf;
xdf=linspace(-1+h,1-h,mdf-1)';
A=toeplitz([2,-1,zeros(1,mdf-3)])/h^2+c*eye(mdf-1,mdf-1);
B=SPE_fbe(xdf); ydf=A\B;
%
% Graphical display
%
plot (xa,SPE_special(xa),xa,y,'--',xdf,ydf,'x')
legend('exact','spectral','Finite diff.')
fprintf('\n spectral method error= %e \n finite diff. error= %e\n ',...
                              es,norm(ydf-SPE_special(xdf),inf));
