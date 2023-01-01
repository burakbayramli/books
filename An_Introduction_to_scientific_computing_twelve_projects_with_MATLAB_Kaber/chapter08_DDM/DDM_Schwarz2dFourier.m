%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [conviter,cpu,mem,n2,b2]=DDM_Schwarz2dFourier(n1,ns,no,a1, a2,b1,b2,...
						   f,f1,g1,detailed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [conviter,cpu,mem,n2,b2]=DDM_Schwarz2dFourier(n1,ns,no,a1, a2,b1,b2,...
%%                                                   f,f1,g1,detailed)
%% Exercise 8.4
%% Schwarz  method of domain decomposition with overlapping for the finite differences
%% solution of the boundary conditions problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  Dirichlet b.c. on edges //x1
%%  Fourier and Neumann b.c. on edges  // x2
%%  du/dx1(a1,x2)+ca(u-uext)=0          u(x1,a2)=f1(x1)
%%  du/dx1(b1,x2)+cb(u-uext)=0          u(x1,b2)=g1(x1)
%%
%% Input parameters:
%%      n1 : number of cells  on [a1,b1]
%%      ns  : number of subdomains in the x2 direction
%%      no : number of cells in overlapping region
%%      a1, a2, b1, b2: minimal and maximal abscissa and ordinates of the
%%                  rectangular domain
%%      f : right hand side function of the problem 
%%      f1, g1 : functions defining the non homogeneous Dirichlet
%%      boundary conditions 
%       detailed: non zero to have intermediate graohical displays.
%% 
%%                    
%% Output parameters:
%%      conviter: number of iterations
%%      cpu     : computing time
%%      mem     : memory   needed to store the solution and the matrix
%%      n2      : number of points per subdomain in the x2 direction
%%      b2      : total size of domain in the x2 direction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tic  % computing time counter start
h=(b1-a1)/(n1+1);
n2tot=round(b2/h);        % there are n2tot cells in total domain, 
n2=round((n2tot-no*(1-ns))/ns)-1;  % and n2+1 cells in each subdomain
n2tot= ns*(n2+1)+no*(1-ns);
b2=a2+h*n2tot;          % final global size
% memory   needed to store the solution and the matrix
mem=(n1*n2)^2+ns*n1*n2; 
if detailed,
  fprintf('%d identical subdomains of width %d cells in x1 direction:\n',ns,n1);
  fprintf('Each subdomains %d cells in the x2 direction\n',n2+1);
  fprintf('The overlapping regions have %d cells in the x2 direction\n',no)
  fprintf('The total size is %f on %d cells in the x2 direction\n',b2,n2tot);
  fprintf('The memory needed to store the solution and the matrix is %d\n',mem);
  %
end
%
% the size of each subdomain is n11 x n2
%
n11=n1+2;
%
% fixed noudary conditions and right hand sides are prepared in arrays
RHS=zeros(n11*n2,ns);
starts=0; % subdomain index start
Rhsm=zeros(n11,n2);
Solm=zeros(n11,n2);
Ua2l=feval(f1,a1+h*[1:n11])' ; % boundary condition on edge x2=a2
Ub2r=feval(g1,a1+h*[1:n11])';  %  boundary condition on edgex2=b2
for s=1:ns
  RHS(:,s)=DDM_RightHandSide2dFourier(f,h,n11,n2,a1,a2+starts*h);
  Solm(:,no)=(s*Ub2r+(ns-s)*Ua2l)/ns;  
  Solcol(:,s)=Solm(:);
  starts=starts+n2+1-no;
end
Rhsm=zeros(n11,n2);
Lapl=-DDM_LaplaceFourier(h,n1,n2);   
maxiter=100; conviter=1; err=1; epsilon=0.001;
Solcol=zeros(n11*n2,ns);
ERR=[];
while err>epsilon & conviter<maxiter
  if detailed,
     hold off;    pause(0.1);    fprintf('iteration number %d \n',conviter);
  end
  starts=0;
  err=0;
  Ua2=Ua2l; % the left edge in x2 contains n1+2 lines
  for s=1:ns  
    if s<ns   % Boundary condition on right edge is obtained
   % from the solution at previous iteration on the right hand side 
      % neighboring subdomain 
    Solmr=zeros(n11,n2);Solmr(:)=Solcol(:,s+1);
      Ub2=Solmr(:,no);
    else
      Ub2=Ub2r; % the exact boundary condition is used on the right edge
    end
    %                
    Rhsm(:,1)=  Ua2/h^2;
    Rhsm(:,n2)= Ub2/h^2;
    Rhscol=RHS(:,s)+Rhsm(:);   
    Solcol(:,s)=Lapl\ Rhscol;    
    Solm=zeros(n11,n2);Solm(:)=Solcol(:,s);
   % The boundary condition on the left edge is obtained for the following
  % subdomain
    Ua2=Solm(:,n2-no+1);
    if s>1 % the overlapping region is extracted  
      OVER=Solml(:,n2-no+2:n2)-Solm(:,1:no-1);      
      err=max(err,norm(OVER(:),inf));
    end
    Solml=Solm;  
    if detailed,
      surf(a2+h*starts+h*[1:n2],a1+h*[1:n11],Solm);
      title(strcat('iteration ',int2str(conviter)))
      if s==1 
        hold on
      end
    end
    starts=starts+n2+1-no;
  end    
  ERR=[ERR,err];
  conviter=conviter+1;
  if detailed,
    fprintf('error =%f at iteration %d\n',err,conviter)
    pause(2)
  end
end
cpu=toc; % computing time counter stop
if detailed, % Visualisation after convergence
  fprintf('convergence after %d iterations in %d seconds\n',conviter,cpu);
  fprintf('convergence after  %d iterations\n',conviter);
  figure;  hold on ;
  Solmr=zeros(n11,n2);Solmr(:)=Solcol(:,1); 
  starts=0;
  Solmr=[Ua2l,Solmr]; % exact boundary condition on left edge
  surf(a2+h*starts+h*[0:n2],a1+h*[0:n1+1],Solmr);
  starts   =starts+n2+1-no;
  for s=2:ns-1
    Solmr=zeros(n11,n2);Solmr(:)=Solcol(:,s); 
    surf(a2+h*starts+h*[1:n2],a1+h*[0:n1+1],Solmr);
    starts=starts+n2+1-no;
  end 
  Solmr=zeros(n11,n2);Solmr(:)=Solcol(:,ns); 
  Solmr=[Solmr,Ub2r];  %exact boundary condition on right edge
  surf(a2+h*starts+h*[1:n2+1],a1+h*[0:n1+1],Solmr);
  title('Final solution')
end
