%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [conviter,cpu,mem,n2,b2]=DDM_Schwarz2dDirichlet(n1,ns,no,...
		     a1, a2,b1,b2,f,f1,g1,f2,g2,detailed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function [conviter,cpu,mem,n2,b2]=DDM_Schwarz2dDirichlet(n1,ns,no,...
%%		   a1, a2,b1,b2,f1,f1,g1,f2,g2,detailed)
%% Exercise 8.4
%% Schwarz  method of domain decomposition with overlapping for the 
%% finite differences solution of the boundary conditions problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  + Dirichlet b. c.
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1) 
%%
%% Input parameters:
%%    n1 : number of cells  on [a1,b1]
%%    ns  : number of subdomains in the x2 direction
%%    no   : number of cells in overlapping region
%%    a1, a2, b1, b2: minimal and maximal abscissa and ordinates of
%%                  the rectangular domain
%%    f : right hand side function of the problem 
%%    f1, g1,f2,g2 : functions defining the non homogeneous Dirichlet 
%%      boundary conditions on the four edges of the domain
%%    detailed: non zero to have intermediate graohical displays.
%% 
%% Output parameters:
%%    conviter: number of iterations
%%    cpu     : computing time
%%    mem     : memory   needed to store the solution and the matrix
%%    n2      : number of points per subdomain in the x2 direction
%%    b2      : total size of domain in the x2 direction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tic  % computing time counter start
h=(b1-a1)/(n1+1);
n2tot=round(b2/h);        % there are n2tot cells in total domain, 
n2=round((n2tot-no*(1-ns))/ns)-1; % and n2+1 cells in each subdomain
n2tot= ns*(n2+1)+no*(1-ns);
b2=a2+h*n2tot;          % final global size
% memory   needed to store the solution and the matrix
mem=(n1*n2)^2+ns*n1*n2; 
if detailed,
  fprintf('%d ident. s.d. of width %d cells in x1 direction:\n',ns,n1);
  fprintf('Each subdomains %d cells in the x2 direction\n',n2+1);
  fprintf('The overlap. regions have %d cells in the x2 direction\n',no)
  fprintf('The total size is %f on %d cells in the x2 direction\n',b2,n2tot);
  fprintf('The memory needed to store the solution and the matrix is %d\n',mem); 
end
%
% the size of each subdomain is  n1 x n2
%
a11=a1-h;  % Dirichlet condition on the edge // to X2 
%
% Boundary conditions independant of iterations are set in arrays
Ua2l=feval(f1,a1+h*[1:n1])' ; % boundary condition on edge x2=a2
Ub2r=feval(g1,a1+h*[1:n1])';  % boundary condition on edge  x2=b2
% The right hand side on each subdomain is an array to which the 
% contribution of internal edges will be added at each iteration
RHS=zeros(n1*n2,ns);
starts=0; % starting index of subdomain s
Rhsm=zeros(n1,n2);
Solm=zeros(n1,n2);
for s=1:ns
  RHS(:,s)=DDM_RightHandSide2dDirichlet(f,h,n1,n2,a1,a2+starts*h);
  % Dirichlet boundary condition on edge x1=a1  
  Ua1(s,:)=feval(f2,a2+starts*h+[1:n2]*h);
  % Dirichlet boundary condition on edge  x1=b1
  Ub1(s,:)=feval(g2,a2+starts*h+[1:n2]*h);
  Rhsm(1,:)=Ua1(s,:)/h^2;
  Rhsm(n1,:)=Ub1(s,:)/h^2;
  RHS(:,s)=  RHS(:,s)+Rhsm(:);
  Solm(:,no)=(i*Ub2r+(ns-s)*Ua2l)/ns;  
  Solcol(:,s)=Solm(:);
  starts=starts+n2+1-no;
end
Rhsm=zeros(n1,n2);
Lapl=-DDM_LaplaceDirichlet(h,n1,n2);   
maxiter=100; conviter=1; err=1; epsilon=0.001;
Solcol=zeros(n1*n2,ns);
ERR=[];
while err>epsilon & conviter<maxiter
  if detailed,
     hold off;    
     pause(0.1);    
     fprintf('iteration number %d \n',conviter);
  end
  starts=0;
  err=0;
  Ua2=Ua2l; %left edge in x2 contains n1+2 lines
  for s=1:ns  
    if s<ns   % The bound. condition on the right edge is obtained 
      % from the solution at previous iteration on the right
      %  hand side  neighboring subdomain 
      Solmr=zeros(n1,n2);Solmr(:)=Solcol(:,s+1);
      Ub2=Solmr(:,no);
    else
      Ub2=Ub2r; % the exact bound. cond. is used on the right edge.
    end
    %                
    Rhsm(:,1)=  Ua2/h^2;
    Rhsm(:,n2)= Ub2/h^2;
    Rhscol=RHS(:,s)+Rhsm(:);   
    Solcol(:,s)=Lapl\ Rhscol;    
    Solm=zeros(n1,n2);Solm(:)=Solcol(:,s);
  % The boundary condition on the left edge is obtained for the 
  % next subdomain on the right
    Ua2=Solm(:,n2-no+1);
    if s>1 % the overlapping region is extracted 
      OVER=Solml(:,n2-no+2:n2)-Solm(:,1:no-1);      
      err=max(err,norm(OVER(:),inf));
    end
    Solml=Solm;  
    if detailed,
      surf(a2+h*starts+h*[1:n2],a11+h*[1:n1],Solm);
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
cpu=toc; % computing time counter starts here
if detailed, % Visualization after convergence
  Ua2l=[feval(f1,a1); Ua2l;feval(f1,b1)] ;
  Ub2r=[feval(g1,a1); Ub2r;feval(g1,b1)] ; 
  fprintf('convergence after %d iterations in %d seconds\n',...
                       conviter,cpu);
  figure;  hold on ;
  Solmr=zeros(n1,n2);Solmr(:)=Solcol(:,1); 
  % In the case of  Dirichlet bc on edges // to x2 two lines 
  % corresponding to the boundary conditions on x1=a1 and x1=b1 are 
  % added to the solution on each subdomain
  starts=0;
  Solmr= [Ua1(1,:);Solmr;Ub1(1,:)]; % Dirichlet bc  
  Solmr=[Ua2l,Solmr]; % exact boundary condition on left edge
  surf(a2+h*starts+h*[0:n2],a1+h*[0:n1+1],Solmr);
  starts   =starts+n2+1-no;
  for s=2:ns-1
    Solmr=zeros(n1,n2);Solmr(:)=Solcol(:,s); 
    Solmr= [Ua1(s,:);Solmr;Ub1(s,:)];
    surf(a2+h*starts+h*[1:n2],a1+h*[0:n1+1],Solmr);
    starts=starts+n2+1-no;
  end 
  Solmr=zeros(n1,n2);Solmr(:)=Solcol(:,ns); 
  Solmr= [Ua1(ns,:);Solmr;Ub1(ns,:)];
  Solmr=[Solmr,Ub2r];  %exact boundary condition on right edge
  surf(a2+h*starts+h*[1:n2+1],a1+h*[0:n1+1],Solmr);
  title('Final solution')
end
