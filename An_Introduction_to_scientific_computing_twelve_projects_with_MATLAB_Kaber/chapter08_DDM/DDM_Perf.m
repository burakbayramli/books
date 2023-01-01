%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 8.5
%% Analyze of the convergence of the Schwarz  method of 
%% domain decomposition with overlapping for the finite differences
%% solution of the boundary conditions problem
%%  -nabla u=f  on [a1,b1]x[a2,b2]
%%  + Dirichlet b. c.
%%  u(a1,x2)=f2(x2)          u(x1,a2)=f1(x1)
%%  u(b1,x2)=g2(x2)          u(x1,b2)=g1(x1) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
close all
global a1
global a2
global b1
global b2
n1=10;  a1=0;  a2=0; b1=1;     b2exact=50;   
h=(b1-a1)/(n1+1); 
no=10;       % the overlapping region has fixed size
Iter=[];Mem=[];B2=[];Cpu=[];N2=[];NS=[];
for ns=5:80    % All reasonable values for the number of subdomains are tried 
    % the domain decomposition must be feasible
   h=(b1-a1)/(n1+1);
   n2tot=round(b2exact/h);              % there are n2tot cells alltogether, 
   n2=round((n2tot-no*(1-ns))/ns)-1;% and n2+1 cells in each subdomain
   n2tot= ns*(n2+1)+no*(1-ns);
   b2=a2+h*n2tot;  % final total size of the domain
   if abs(b2-b2exact)<0.0001
     NS=[NS,ns];
     fprintf('%d sd overlap=%d n2=%d n2tot=%d\n',ns,no,n2,n2tot);
     [iter,cpu,mem,n2,b2]=DDM_Schwarz2dDirichlet(n1,ns,no,a1, a2,b1,b2,'DDM_rhs2dExact',...
        'DDM_f1Exact','DDM_g1Exact','DDM_f2Exact','DDM_g2Exact',0);
     Iter=[Iter,iter];
     Mem=[Mem,mem];
     Cpu=[Cpu,cpu];       
     N2=[N2,n2];
   end
end
close all
subplot(4,1,1); plot(NS,Iter,'-o'); legend ('number of iterations');
title('Influence of the subdomains number')
subplot(4,1,2); plot(NS,Mem,'-o');legend ('memory');
subplot(4,1,3); plot(NS,Cpu,'-o');legend ('computation time');
subplot(4,1,4); plot(NS,N2,'-o');legend ('size of subdomains');
