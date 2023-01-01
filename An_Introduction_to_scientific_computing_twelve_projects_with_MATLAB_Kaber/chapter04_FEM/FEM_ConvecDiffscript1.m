%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
eps=0.1;lambda=1;                    %physical parameters

f = inline('ones(size(x))');          %right-hand side of the equation
n=10;
A=FEM_ConvecDiffAP1(eps,lambda,n);     %matrix of the linear system
b=FEM_ConvecDiffbP1(n,f);              %right-hand side of the linear system
u=A\b;                                %FEM solution 
u=[0;u;0];                            %add to u the boundary values
x=(0:n+1)/(n+1);                      %mesh
uexa=FEM_ConvecDiffSolExa(eps,lambda,1,x);%exact solution computed on x
figure(1);plot(x,uexa,x,u,'+-r','MarkerSize',10,'LineWidth',2)
set(gca,'XTick',0:.2:1,'FontSize',20)
set(gca,'YTick',0:.2:1)
title('\fontsize{16}Solution of the convection diffusion problem, \lambda =1,\epsilon=0.1, n=10');
%
