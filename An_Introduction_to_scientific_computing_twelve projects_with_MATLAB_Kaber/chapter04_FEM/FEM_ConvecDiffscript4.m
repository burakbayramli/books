%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=10;eps=0.01;lambda=1;
A=FEM_ConvecDiffAP2(eps,lambda,n);
a=A(2:2:2*n+1,2:2:2*n+1);b=A(2:2:2*n+1,1:2:2*n+1);
c=A(1:2:2*n+1,2:2:2*n+1);d=A(1:2:2*n+1,1:2:2*n+1);
f = inline('ones(size(x))');          
sm=FEM_ConvecDiffbP2(n,f);  
nsm=sm(2:2:2*n+1)-b*inv(d)*sm(1:2:2*n+1);
u=(a-b*inv(d)*c)\nsm; %computation of v                    
x=linspace(0,1,100);
uexa=FEM_ConvecDiffSolExa(eps,lambda,1,x);
plot(x,uexa);hold on
plot((1:n)/(n+1),u,'+');hold off;
