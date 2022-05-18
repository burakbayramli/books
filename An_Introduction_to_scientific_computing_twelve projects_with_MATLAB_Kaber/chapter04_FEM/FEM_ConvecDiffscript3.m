%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
eps=0.01;lambda=1;                    
f = inline('ones(size(x))');          
yes=1;
while yes
  n=input('enter n : ');
  A=FEM_ConvecDiffAP1(eps,lambda,n);      
  b=FEM_ConvecDiffbP1(n,f);              
  u=A\b;                          
  u=[0;u;0];                       
  x=(0:n+1)/(n+1);                      
  uexa=FEM_ConvecDiffSolExa(eps,lambda,1,x);
  plot(x,uexa,x,u,'+-r')
  Peclet=abs(lambda)/2/eps/(n+1)
  yes=input('more ? yes=1, no=0 ')
end
