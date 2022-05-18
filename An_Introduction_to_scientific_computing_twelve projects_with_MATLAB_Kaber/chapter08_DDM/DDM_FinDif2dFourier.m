%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [n2,b2, Solm]=DDM_FinDif2dFourier(n1,a1,a2,b1,b2, f,f1,g1,detailed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [n2,b2, Solm]=DDM_FinDif2dFourier(n1,a1,a2,b1,b2, rhs2d,...
%%                                f1,g1,RightHandSide2d,Laplace,n11,detailed)
%% Exercise 8.3
%%  Finite differences resolution of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  Dirichlet b.c. on edges //x1
%%  Fourier and Neumann b.c. on edges // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%%
%% Input parameters:
%%      n1 :  number of cells  on  [a1,b1]
%%      a1, a2, b1, b2: minimal and maximal abscissa and ordinates of the
%%                  rectangular domain
%%      f :  right hand side function of the problem 
%%      f1,g1 : functions defining the non homogeneous Dirichlet
%%      boundary conditions on the edges of the domain //x1
%       detailed: non zero to have intermediate graohical displays.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h=(b1-a1)/(n1+1);
n2tot=round((b2-a2)/h)-1;
b2=a2+h*(n2tot+1);  % eventually, final size of the domain
n2=n2tot;
if detailed,
  fprintf('h step in both directions %f:\n',h);
  fprintf('%d points in x1 %d points in x2 \n',n1,n2);
  fprintf('final dimension in x2 direction= %f\n',b2);
%
end
%
% n11 is the number of degrees of freedom in the x1 direction.
n11=n1+2;
% the linear system size is n11 x n2
SM=zeros(n11*n2);
%  boundary conditions:
Ua2=feval(f1,a1+h*[0:n1+1])' ;
Ub2=feval(g1,a1+h*[0:n1+1])'; 
% Laplacian matrix
Lapl=-DDM_LaplaceFourier(h,n1,n2);   
% Right Hand side :
RHS=DDM_RightHandSide2dFourier(f,h,n11,n2,a1,a2);
% Boundary conditions influence on the right hand side
Rhsm=zeros(n11,n2);
Rhsm(:,1)=  Rhsm(:,1)+Ua2/h^2;
Rhsm(:,n2)=  Rhsm(:,n2)+Ub2/h^2;
Rhscol=RHS+Rhsm(:);   
Solcol=Lapl\ Rhscol;      
Solm=zeros(n11,n2);
Solm(:)=Solcol;
Solm=[Ua2,Solm,Ub2]; % exact boundary condition on the left edge
if detailed
  surf(a2+h*[0:n2+1],a1+h*[0:n1+1],Solm);
  title('Finite difference 2D Laplacian solution')
end
