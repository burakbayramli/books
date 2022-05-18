%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [n2,b2, Solm]=DDM_FinDif2dDirichlet(n1,a1,a2,b1,b2, ...
					 f,f1,g1,f2,g2,detailed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [n2,b2, Solm]=DDM_FinDif2dDirichlet(n1,a1,a2,b1,b2, sm2d,...
%%					    f1,g1,f2,g2,detailed)
%% Exercise 8.3
%%  Finite differences resolution  of the boundary problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1)
%%
%% Input parameters:
%%      n1 : number of cells  on [a1,b1]
%%      a1, a2, b1, b2: minimal and maximal abscissa and ordinates of the
%%                  rectangular domain
%%      f : right hand side function of the problem 
%%      f1, g1,f2,g2 : functions defining the non homogeneous Dirichlet
%%      boundary conditions on the four edges of the domain
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
% Laplacian matrix
Lapl=-DDM_LaplaceDirichlet(h,n1,n2);   
%
% Right hand side :
RHS=DDM_RightHandSide2dDirichlet(f,h,n1,n2,a1,a2);
%
% Boundary conditions influence on the right hand side
Ua1=feval(f2,a2+[1:n2]*h); 
Ub1=feval(g2,a2+[1:n2]*h);      
Ua2=feval(f1,a1+h*[0:n1+1])' ;
Ub2=feval(g1,a1+h*[0:n1+1])'; 
Rhsm=zeros(n1,n2);
Rhsm(1,:)=Ua1/h^2;
Rhsm(n1,:)=Ub1/h^2;
Rhsm(:,1)=  Rhsm(:,1)+Ua2(2:n1+1)/h^2;
Rhsm(:,n2)=  Rhsm(:,n2)+Ub2(2:n1+1)/h^2;
Rhscol=RHS+Rhsm(:);   
%
Solcol=Lapl\ Rhscol;      
Solm=zeros(n1,n2);
Solm(:)=Solcol;
% In the case of Dirichlet bc on the edge // to x2 two lines corresponding
% to the boundary conditions on x1=a1 and x1=b1 are added to the solutions 
% on each subdomain
Solm= [Ua1;Solm;Ub1]; % Dirichlet bc
% Boundary condition on the edge // to x1
Solm=[Ua2,Solm,Ub2]; % exact boundary condition on the left edge
if detailed
  surf(a2+h*[0:n2+1],a1+h*[0:n1+1],Solm);
  title('Finite difference 2D Laplacian solution')
end
